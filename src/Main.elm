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

type alias Tag = String


type alias Model =
  { ping : TaggedPing
  , isCommitted : Bool
  , omnibarContents : String
  }

type alias TaggedPing =
  { questions : List Question
  , miscTags : Set Tag
  , ping : Ping
  , selectedTags : Set Tag
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
    | Commit
    | SetOmnibar String
    | ExecuteOmnibar
    | Ignore

init : () -> ( Model , Cmd Msg )
init () =
  ( { ping = examplePing
    , isCommitted = True
    , omnibarContents = ""
    }
  , focusOnOmnibar
  )

focusOnOmnibar : Cmd Msg
focusOnOmnibar =
  Task.attempt
    (always Ignore)
    (Browser.Dom.focus "omnibar")

update : Msg -> Model -> ( Model , Cmd Msg)
update msg model =
    -- Debug.log (Debug.toString (msg, model)) <|
    case msg of
        Toggle tag ->
            ( { model
              | ping = model.ping |> toggle tag
              , isCommitted = False
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
            ( { model | isCommitted = True }
            , Cmd.none -- TODO: implement persistence
            )
        Ignore ->
            ( model , Cmd.none )

omnibarCandidate : Model -> Maybe String
omnibarCandidate {ping, omnibarContents} =
  if String.isEmpty omnibarContents then
    Nothing
  else
    Set.toList (allTags ping)
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
    {ping, isCommitted, omnibarContents} = model
    candidate = omnibarCandidate model
    -- _ = Debug.log "candidate" candidate

    selectedTags = ping.selectedTags
    unselectedTags = Set.diff (allTags ping) ping.selectedTags
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
      [ H.text <| "Ping for: " ++ localTimeString Time.utc (TagTime.toTime ping.ping)
      , H.br [] []
      , H.button
        [ HE.onClick Commit
        , HA.disabled isCommitted
        ]
        [ H.text "Commit" ]
      , H.input
          [ HA.id "omnibar"
          , HA.value omnibarContents
          , HE.onInput SetOmnibar
          , HE.on "keydown"
            <| JD.map2
                (\ctrl keyCode -> Debug.log (Debug.toString (ctrl, keyCode)) <| case (ctrl, keyCode) of
                  (False, 13) -> ExecuteOmnibar
                  (True, 13) -> Commit
                  _ -> Ignore
                )
                (JD.field "ctrlKey" JD.bool)
                HE.keyCode
          ]
          []
      , relevantQuestions ping
        |> List.map (viewQuestion tagAttrs ping.selectedTags)
        |> List.map (\h -> H.li [] [h])
        |> H.ul []
      , allTags ping
        |> Set.toList
        |> List.sort
        |> List.map (viewTag tagAttrs)
        |> H.div []
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

main = Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
    }





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
  }
