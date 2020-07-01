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
    | Ignore

init : () -> ( Model , Cmd Msg )
init () =
  ( { ping = examplePing
    , isCommitted = True
    }
  , focusOnShortcuts
  )

focusOnShortcuts : Cmd Msg
focusOnShortcuts =
  Task.attempt
    (always Ignore)
    (Browser.Dom.focus "shortcut-input")

update : Msg -> Model -> ( Model , Cmd Msg)
update msg model =
    case msg of
        Toggle tag ->
            ( { model
              | ping = model.ping |> toggle tag
              , isCommitted = False
              }
            , focusOnShortcuts
            )
        Commit ->
            ( { model | isCommitted = True }
            , Cmd.none -- TODO: implement persistence
            )
        Ignore ->
            ( model , Cmd.none )

view : Model -> Html Msg
view {ping, isCommitted} =
  H.div []
    [ H.text <| "Ping for: " ++ localTimeString Time.utc (TagTime.toTime ping.ping)
    , H.br [] []
    , H.button
      [ HE.onClick Commit
      , HA.disabled isCommitted
      ]
      [ H.text "Commit" ]
    , relevantQuestions ping
      |> List.map (viewQuestion ping.selectedTags)
      |> List.map (\h -> H.li [] [h])
      |> H.ul []
    , allTags ping
      |> Set.toList
      |> List.sort
      |> List.map (\t -> viewTag (Set.member t ping.selectedTags) t)
      |> H.div []
    ]

viewQuestion : Set Tag -> Question -> Html Msg
viewQuestion selectedTags question =
  H.div []
    [ H.text question.prompt
    , H.text " "
    , question.responses
      |> List.map (\tag -> viewTag (Set.member tag selectedTags) tag)
      |> List.intersperse (H.text " ")
      |> H.span []
    ]

viewTag : Bool -> Tag -> Html Msg
viewTag selected tag =
  H.button
    [ HE.onClick (Toggle tag)
    , HA.style "background-color" (if selected then "lightgreen" else "#eeeeee")
    ]
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
