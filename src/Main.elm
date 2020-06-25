module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, span, div, form, label, input, text)
import Html.Attributes exposing (attribute, name, type_, checked, value, disabled, id)
import Html.Events exposing (onClick, onInput, onCheck, onSubmit)

import Dict exposing (Dict)
import Set exposing (Set)

import Prompts



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias Prompt = String
type alias Option = String
type alias Question =
    { options : List Option
    , followups : Answer -> List Prompt
    }
type alias Answer = { selected : Set Option , notes : String }
type PartialAnswer
    = Tentative Answer
    | Finalized Answer

emptyAnswer : PartialAnswer
emptyAnswer =
    Tentative { selected = Set.empty , notes = "" }

type alias Model =
    { questionAnswers : Dict Prompt { question : Question , answer : PartialAnswer}
    , rootQuestions : List Prompt
    }

isDone : Model -> Prompt -> Bool
isDone model prompt =
    case Dict.get prompt model.questionAnswers of
        Nothing -> False
        Just {answer} ->
            case answer of
                Tentative _ -> False
                Finalized _ -> True

allQuestions : Dict Prompt Question
allQuestions =
    Dict.fromList
        [ ( Prompts.areYouAwake
          , { options=["no"] -- TODO: want True/False
            , followups=(\ans -> if Set.member "no" ans.selected then [] else [Prompts.whoAreYouWith])
            }
          )
        , ( Prompts.whoAreYouWith
          , { options=["Rebecca","Yam"]
            , followups=(always [])
            }
          )
        ]

init : Model
init =
    { rootQuestions = [Prompts.areYouAwake]
    , questionAnswers = Dict.map (\k v -> {question=v, answer=emptyAnswer}) allQuestions
    }



-- UPDATE


type Msg
    = Finalize Prompt
    | Definalize Prompt
    | SetSelected Prompt Option Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        Finalize prompt ->
            Debug.log ("finalizing " ++ prompt) <|
            { model
            | questionAnswers = model.questionAnswers
                |> Dict.update prompt (Maybe.map (\qa -> { qa | answer = qa.answer |> (\partialAnswer -> case partialAnswer of
                        Tentative ans -> Finalized ans
                        Finalized ans -> Finalized ans |> Debug.log "ignoring Finalize because already finalized"
                    )}))
            }

        Definalize prompt ->
            Debug.log ("definalizing " ++ prompt) <|
            { model
            | questionAnswers = model.questionAnswers
                |> Dict.update prompt (Maybe.map (\qa -> { qa | answer = qa.answer |> (\partialAnswer -> case partialAnswer of
                        Tentative ans -> Tentative ans |> Debug.log "ignoring Finalize because already finalized"
                        Finalized ans -> Tentative ans
                    )}))
            }

        SetSelected prompt option value ->
            { model
            | questionAnswers = model.questionAnswers
                |> Dict.update prompt (Maybe.map (\qa -> { qa | answer = qa.answer |> (\partialAnswer -> case partialAnswer of
                        Tentative ans -> Tentative { ans | selected = ans.selected |> (if value then Set.insert option else Set.remove option) }
                        Finalized ans -> Finalized ans |> Debug.log "refusing to modify finalized answer"
                    )}))
            }




-- VIEW

view : Model -> Html Msg
view model =
    let
        reachable = reachablePrompts model
        todo = List.filter (not << isDone model) reachable
        done = Set.toList <| Set.diff (Set.fromList reachable) <| Set.fromList todo
    in
        (todo ++ done)
        |> List.map (\p -> case Dict.get p model.questionAnswers of
            Just {question, answer} -> viewQuestionAnswer p question answer
            Nothing -> Debug.todo "impossible"
            )
        |> div []

viewQuestionAnswer : Prompt -> Question -> PartialAnswer -> Html Msg
viewQuestionAnswer prompt question partialAnswer =
    case partialAnswer of
        Tentative answer ->
            form [onSubmit (Finalize prompt)]
                [ text prompt
                , question.options
                  |> List.map (\option -> viewCheckbox prompt option [checked (Set.member option answer.selected)])
                  |> div []
                , input [type_ "submit", value "Finalize"] []
                ]
        Finalized answer ->
            form [onSubmit (Definalize prompt)]
                [ text prompt
                , question.options
                  |> List.map (\option -> viewCheckbox prompt option [checked (Set.member option answer.selected), disabled True])
                  |> div []
                , input [type_ "submit", value "Re-answer"] []
                ]


viewCheckbox : Prompt -> Option -> List (Attribute Msg) -> Html Msg
viewCheckbox prompt option attrs =
    let id_ = prompt ++ option in
    span []
        [ input (attrs ++ [ type_ "checkbox" , onCheck (SetSelected prompt option) , id id_ ]) []
        , label [ attribute "for" id_ ] [ text option ]
        ]

reachablePrompts : Model -> List Prompt
reachablePrompts model =
    let
        descendants : Prompt -> List Prompt
        descendants prompt =
            Debug.log ("descendants of " ++ prompt) <|
            case Dict.get prompt model.questionAnswers of
                Just {question, answer} ->
                    case answer of
                        Tentative _ -> [prompt]
                        Finalized ans -> prompt :: List.concatMap descendants (question.followups ans)
                Nothing -> Debug.todo ("prompt missing: " ++ prompt)
    in
        List.concatMap descendants model.rootQuestions
