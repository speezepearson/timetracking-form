module Main exposing
    ( ..
    )

import Browser
import Browser.Dom
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Json.Encode as JE
import Bytes exposing (Bytes)
import Task
import Time
import Http

import Dict exposing (Dict)
import Set exposing (Set)

import TagTime exposing (Ping)
import FocusedList as FL exposing (FocusedList)

type alias Tag = String

type MetaModel
  = Initializing
  | Initialized Model

type alias Model =
  { pings : FocusedList TaggedPing
  , omnibarContents : String
  , timeZone : Time.Zone
  }

type alias TaggedPing =
  { questions : List Question
  , miscTags : Set Tag
  , ping : Ping
  , selectedTags : Set Tag
  , isCommitted : Bool
  }

type alias Question =
  { relevance : Relevance
  , prompt : String
  , responses : List Tag
  }

type Relevance
  = AlwaysRelevant
  | RelevantIf Tag
  | RelevantIfNot Tag
  | RelevantIfBoth Relevance Relevance



-- BUSINESS LOGIC

isRelevant : Relevance -> Set Tag -> Bool
isRelevant relevance set =
  case relevance of
    AlwaysRelevant -> True
    RelevantIf tag -> Set.member tag set
    RelevantIfNot tag -> not (Set.member tag set)
    RelevantIfBoth r1 r2 -> isRelevant r1 set && isRelevant r2 set

relevantQuestions : TaggedPing -> List Question
relevantQuestions {questions, selectedTags} =
  List.filter
    (\{relevance} -> isRelevant relevance selectedTags)
    questions

allTags : TaggedPing -> Set Tag
allTags {questions, miscTags} =
  List.foldl
    (\q tags -> Set.union tags (Set.fromList q.responses))
    miscTags
    questions

toggle : Tag -> TaggedPing -> TaggedPing
toggle tag ping =
  { ping
  | selectedTags =
      ping.selectedTags
      |> (if Set.member tag ping.selectedTags then Set.remove else Set.insert) tag
  }

markCommitted : TaggedPing -> TaggedPing
markCommitted ping =
  { ping | isCommitted = True }

markUncommitted : TaggedPing -> TaggedPing
markUncommitted ping =
  { ping | isCommitted = False }


-- SERDE

encodeRelevance : Relevance -> JE.Value
encodeRelevance relevance =
  case relevance of
    AlwaysRelevant -> JE.list JE.string ["always"]
    RelevantIf tag -> JE.list JE.string ["if", tag]
    RelevantIfNot tag -> JE.list JE.string ["ifNot", tag]
    RelevantIfBoth r1 r2 -> JE.list identity [JE.string "ifBoth", encodeRelevance r1, encodeRelevance r2]

relevanceDecoder : JD.Decoder Relevance
relevanceDecoder =
  JD.map2 Tuple.pair
    (JD.index 0 JD.string)
    (JD.list JD.value |> JD.map (\args -> List.length args - 1))
  |> JD.andThen (\(kind, nargs) -> case (kind, nargs) of
      ("always", 0) ->
        JD.succeed AlwaysRelevant
      ("if", 1) ->
        JD.map RelevantIf (JD.index 1 JD.string)
      ("ifNot", 1) ->
        JD.map RelevantIfNot (JD.index 1 JD.string)
      ("ifBoth", 2) ->
        JD.map2 RelevantIfBoth (JD.index 1 relevanceDecoder) (JD.index 2 relevanceDecoder)
      _ ->
        JD.fail <| "unknown kind/nargs " ++ kind ++ "/" ++ String.fromInt nargs
    )


-- UI STUFF

type Msg
    = Toggle Tag
    | Initialize Time.Zone Time.Posix
    | Commit
    | Later
    | Earlier
    | SetOmnibar String
    | ExecuteOmnibar
    | GotPing Ping
    | Ignore

main = Browser.element
    { init = metaInit
    , view = metaView
    , update = metaUpdate
    , subscriptions = (\_ -> Sub.none)
    }

metaInit : () -> ( MetaModel , Cmd Msg )
metaInit () =
  ( Initializing
  , Cmd.batch
      [ Task.perform identity
        <| Task.map2 Initialize Time.here Time.now
      ]
  )

metaView : MetaModel -> Html Msg
metaView metamodel =
  case metamodel of
    Initializing -> H.text "Initializing..."
    Initialized model -> view model

metaUpdate : Msg -> MetaModel -> ( MetaModel , Cmd Msg )
metaUpdate msg metamodel =
  case metamodel of
    Initializing ->
      case msg of
        Initialize here now ->
          init here now
          |> Tuple.mapFirst Initialized
        _ ->
          ( metamodel |> Debug.log ("uninitialized; ignoring " ++ Debug.toString msg)
          , Cmd.none
          )
    Initialized model ->
      update msg model
      |> Tuple.mapFirst Initialized


init : Time.Zone -> Time.Posix -> ( Model , Cmd Msg )
init here now =
  let
    ttPing = TagTime.lastBefore (now |> Time.posixToMillis |> (\x -> x - 1000*3600*5) |> Time.millisToPosix)
  in
    ( { pings = FocusedList [] {examplePing | ping = ttPing} []
      , omnibarContents = ""
      , timeZone = here
      }
    , Cmd.batch
        [ Task.perform GotPing (TagTime.waitForPing ttPing)
        , focusOnOmnibar
        ]
    )

focusOnOmnibar : Cmd Msg
focusOnOmnibar =
  Task.attempt
    (always Ignore)
    (Browser.Dom.focus "omnibar")

update : Msg -> Model -> ( Model , Cmd Msg )
update msg model =
    case msg of
        Toggle tag ->
            ( { model
              | pings = model.pings |> FL.updateFocus (toggle tag >> markUncommitted)
              }
            , focusOnOmnibar
            )
        SetOmnibar value ->
            ( { model | omnibarContents = value }
            , Cmd.none
            )
        ExecuteOmnibar ->
            case omnibarCandidate model of
              Nothing -> ( model , Cmd.none )
              Just tag -> update (Toggle tag) { model | omnibarContents = "" }
        Commit ->
            ( { model | pings = model.pings |> FL.updateFocus markCommitted }
            , Cmd.none -- TODO: implement persistence
            )
        Later ->
            ( { model | pings = FL.stepLeft model.pings |> Maybe.withDefault model.pings }
            , Cmd.none
            )
        Earlier ->
            ( { model | pings = FL.stepRight model.pings |> Maybe.withDefault model.pings }
            , Cmd.none
            )
        GotPing ttPing ->
            ( { model | pings = model.pings |> FL.concatLeft [FL.head model.pings |> (\p -> {p | ping = ttPing})] }
            , Task.perform GotPing (TagTime.waitForPing ttPing)
            )
        Ignore ->
            ( model , Cmd.none )

        Initialize _ _ ->
            ( model |> Debug.log ("already initialized; ignoring " ++ Debug.toString msg)
            , Cmd.none
            )

omnibarCandidate : Model -> Maybe String
omnibarCandidate {pings, omnibarContents} =
  if String.isEmpty omnibarContents then
    Nothing
  else
    Set.toList (allTags pings.focus)
    |> List.sort
    |> List.map (\s -> (matchQuality omnibarContents s, s))
    |> List.maximum
    |> Maybe.andThen (\(quality, match) -> if quality <= 0 then Nothing else Just match)

matchQuality : String -> String -> Float
matchQuality =
  let
    helper : {score:Float, streak:Int, midword:Bool} -> String -> String -> Float
    helper state pattern candidate =
      case (String.uncons pattern, String.uncons candidate) of
        (Nothing, Nothing) ->
          state.score
        (_, Nothing) ->
          0
        (Nothing, _) ->
          state.score - (String.length candidate |> toFloat)/(toFloat 1000)
        (Just (p, pRest), Just (c, cRest)) ->
          if p == c then
            let beginningOfWord = not (state.midword && isLower c) in
            helper
              { state
              | score = state.score + toFloat state.streak + (if beginningOfWord then 10 else 0) |> Debug.log (Debug.toString (state, pattern, candidate))
              , midword = not <| Set.member c (Set.fromList <| String.toList "abcdefghijklmnopqrstuvwxyz")
              , streak = state.streak + 1
              }
              pRest
              cRest
          else
            helper
              { state
              | score = state.score
              , midword = isLetter c
              , streak = 0
              }
              pattern
              cRest
  in
    helper { score = 0 , streak = 0 , midword = False}


isLetter : Char -> Bool
isLetter c =
  Set.member c (Set.fromList <| String.toList "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
isLower : Char -> Bool
isLower c =
  Set.member c (Set.fromList <| String.toList "abcdefghijklmnopqrstuvwxyz")



view : Model -> Html Msg
view model =
  let
    {pings, omnibarContents} = model
    candidate = omnibarCandidate model
    -- _ = Debug.log "candidate" candidate

    focus = pings.focus

    selectedTags = focus.selectedTags
    unselectedTags = Set.diff (allTags focus) focus.selectedTags
    candidates = case candidate of
      Just c -> Set.singleton c
      Nothing -> Set.empty

    tagAttrs : Dict Tag (List (H.Attribute Msg))
    tagAttrs =
      Dict.empty
      |> concatValues (candidates     |> Set.foldl (\t -> Dict.insert t [HA.style "outline" "2px dashed red"]) Dict.empty)
      |> concatValues (selectedTags   |> Set.foldl (\t -> Dict.insert t [HA.style "background-color" "lightgreen"]) Dict.empty)
      |> concatValues (unselectedTags |> Set.foldl (\t -> Dict.insert t [HA.style "background-color" "#eeeeee"]) Dict.empty)

    -- _ = Debug.log "candidates" candidates
    -- _ = Debug.log "tagAttrs" tagAttrs

  in
    H.div []
      [ H.input
          [ HA.id "omnibar"
          , HA.value omnibarContents
          , HE.onInput SetOmnibar
          , HE.on "keydown"
            <| JD.map3
                (\ctrl key keyCode -> case (ctrl, key, keyCode) of
                  (False, _, 13) -> ExecuteOmnibar
                  (True, _, 13) -> Commit
                  (False, "ArrowUp", _) -> Later
                  (False, "ArrowDown", _) -> Earlier
                  _ -> Ignore
                )
                (JD.field "ctrlKey" JD.bool)
                (JD.field "key" JD.string)
                HE.keyCode
          ]
          []
      , H.div []
          ( pings.left
            |> List.reverse
            |> List.map (.ping >> TagTime.toTime >> (\t -> "(" ++ localTimeString Time.utc t ++ " UTC) ") >> H.text >> List.singleton >> H.div [])
          )
      , H.hr [] []
      , H.button
        [ HE.onClick Commit
        , HA.disabled focus.isCommitted
        ]
        [ H.text "Commit" ]
      , H.text <| "Ping for: " ++ localTimeString Time.utc (TagTime.toTime focus.ping) ++ " UTC"
      , H.br [] []
      , relevantQuestions focus
        |> List.map (viewQuestion tagAttrs focus.selectedTags)
        |> List.map (\h -> H.li [] [h])
        |> H.ul []
      , allTags focus
        |> Set.toList
        |> List.sort
        |> List.map (viewTag tagAttrs)
        |> H.div []
      , H.hr [] []
      , H.div []
          ( pings.right
            |> List.map (.ping >> TagTime.toTime >> (\t -> "(" ++ localTimeString Time.utc t ++ " UTC) ") >> H.text >> List.singleton >> H.div [])
          )
      ]

concatValues : Dict comparable (List a) -> Dict comparable (List a) -> Dict comparable (List a)
concatValues prefixes =
  Dict.foldl
    (\k v -> Dict.update k (Maybe.withDefault [] >> (++) v >> Just))
    prefixes

viewQuestion : Dict Tag (List (H.Attribute Msg)) -> Set Tag -> Question -> Html Msg
viewQuestion tagAttrs selectedTags question =
  H.div []
    [ H.text question.prompt
    , H.text " "
    , question.responses
      |> List.map (viewTag tagAttrs)
      |> List.intersperse (H.text " ")
      |> H.span []
    ]

viewTag : Dict Tag (List (H.Attribute Msg)) -> Tag -> Html Msg
viewTag attrs tag =
  H.button
    (HE.onClick (Toggle tag) :: (Dict.get tag attrs |> Maybe.withDefault []))
    [ H.text tag ]


localTimeString : Time.Zone -> Time.Posix -> String
localTimeString zone time =
  (String.fromInt <| Time.toYear zone time)
  ++ " " ++ (Debug.toString <| Time.toMonth zone time)
  ++ " " ++ (String.pad 2 '0' <| String.fromInt <| Time.toDay zone time)
  ++ " " ++ (String.pad 2 '0' <| String.fromInt <| Time.toHour zone time)
  ++ ":" ++ (String.pad 2 '0' <| String.fromInt <| Time.toMinute zone time)
  ++ ":" ++ (String.pad 2 '0' <| String.fromInt <| Time.toSecond zone time)





examplePing : TaggedPing
examplePing =
  { ping = TagTime.urPing
  , questions =
      [ { prompt = "Are you asleep?"
        , relevance = AlwaysRelevant
        , responses = ["asleep"]
        }
      , { prompt = "Who are you with?"
        , relevance = RelevantIfNot "asleep"
        , responses = ["with/yam", "with/rebecca"]
        }
      , { prompt = "Are you working?"
        , relevance = RelevantIfNot "asleep"
        , responses = ["working"]
        }
      , { prompt = "What are you working on?"
        , relevance = RelevantIf "working"
        , responses = ["meeting", "coding"]
        }
      ]
  , miscTags = Set.fromList ["miscTag1", "miscTag2"]
  , selectedTags = Set.empty
  , isCommitted = True
  }
