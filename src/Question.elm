module Question exposing
    ( ..
    )

import Browser
import Browser.Dom
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Task

import Dict exposing (Dict)
import Set exposing (Set)
import Tree as T exposing (Tree)
import Tree.Zipper as Z exposing (Zipper)

type alias Prompt = String
type alias Option = String

type alias IsolatedQuestion =
    { prompt : Prompt
    , allOptions : List Option
    , checked : Set Option
    }

check : Option -> IsolatedQuestion -> IsolatedQuestion
check option question =
    { question | checked = question.checked |> Set.insert option }

uncheck : Option -> IsolatedQuestion -> IsolatedQuestion
uncheck option question =
    { question | checked = question.checked |> Set.remove option }

toggle : Option -> IsolatedQuestion -> IsolatedQuestion
toggle option question =
    (if Set.member option question.checked then uncheck else check) option question







type alias Node =
    { question : IsolatedQuestion
    , isFollowup : (Set Option -> IsolatedQuestion -> Bool)
    }

type alias Form = Tree Node

toQuestionTree : Form -> Tree IsolatedQuestion
toQuestionTree form =
    let
        distillLabel : Node -> Node
        distillLabel = identity

        reduce : Node -> List (Tree IsolatedQuestion) -> Tree IsolatedQuestion
        reduce node children =
            T.tree
                node.question
                ( children
                  |> (Debug.log <| "children of " ++ Debug.toString node)
                  |> List.filter (T.label >> node.isFollowup node.question.checked)
                  |> Debug.log "filtered"
                )
    in
        T.restructure
            distillLabel
            reduce
            form



areYouAwake : IsolatedQuestion
areYouAwake =
    { prompt = "Are you awake?"
    , allOptions = ["no"]
    , checked = Set.empty
    }


whoAreYouWith : IsolatedQuestion
whoAreYouWith =
    { prompt = "Who are you with?"
    , allOptions=["Rebecca", "Yam"]
    , checked=Set.empty
    }

type alias Model =
    { focus : Zipper Node
    }

type Msg
    = ToggleChecked Option
    | Focus (Zipper Node)
    | Forward
    | Backward
    | Ignore

init : () -> ( Model , Cmd Msg )
init () =
  ( { focus = Z.fromTree <| T.tree
        { question = areYouAwake
        , isFollowup = (\checked {prompt} ->
            if Set.member "no" checked then
                List.member prompt [whoAreYouWith.prompt]
            else
                False
            )
        }
        [ T.singleton { question = whoAreYouWith , isFollowup = (\_ _ -> False) }
        ]
    }
  , Task.attempt
      (always Ignore)
      (Browser.Dom.focus "shortcut-input")
  )

update : Msg -> Model -> ( Model , Cmd Msg)
update msg model =
    case msg of
        ToggleChecked option ->
            ( { model | focus = model.focus |> Z.mapLabel (\n -> {n | question = n.question |> toggle option}) }
            , Cmd.none
            )
        Focus focus ->
            ( { model | focus = focus |> Debug.log ("focusing on " ++ Debug.toString (Z.label focus)) }
            , Cmd.none
            )
        Forward ->
            ( { model | focus = nextRelevant model.focus |> Maybe.withDefault model.focus }
            , Cmd.none
            )
        Backward ->
            ( { model | focus = prevRelevant model.focus |> Maybe.withDefault model.focus }
            , Cmd.none
            )
        Ignore ->
            ( model , Cmd.none )

isRelevant : Zipper Node -> Bool
isRelevant zipper =
    Debug.log ("is " ++ Debug.toString (Z.label zipper) ++ " relevant?") <|
    case Z.parent zipper of
        Nothing -> True
        Just parent ->
            let
                {question, isFollowup} = Z.label parent
            in
                isFollowup question.checked (Z.label zipper).question && isRelevant parent

nextRelevant : Zipper Node -> Maybe (Zipper Node)
nextRelevant zipper =
    case Z.forward zipper of
        Nothing -> Nothing
        Just next ->
            if isRelevant next then
                Just next
            else
                nextRelevant next

prevRelevant : Zipper Node -> Maybe (Zipper Node)
prevRelevant zipper =
    case Z.backward zipper of
        Nothing -> Nothing
        Just next ->
            if isRelevant next then
                Just next
            else
                nextRelevant next

relevantPredecessors : Zipper Node -> List (Zipper Node)
relevantPredecessors zipper =
    case prevRelevant zipper of
        Nothing -> []
        Just prev -> prev :: relevantPredecessors prev

relevantSuccessors : Zipper Node -> List (Zipper Node)
relevantSuccessors zipper =
    case nextRelevant zipper of
        Nothing -> []
        Just next -> next :: relevantSuccessors next

view : Model -> Html Msg
view {focus} =
    let
        _ = Debug.log "focus" (Z.label focus)
        _ = Debug.log "forward" (Z.forward focus |> Maybe.map Z.label)

        prev = relevantPredecessors focus
        next = relevantSuccessors focus
        _ = Debug.log "next" (List.map (Z.label >> .question >> .prompt) next)
    in
        H.div []
            [ viewActiveQuestion (Z.label focus).question
            , H.div [HA.style "display" "flex"]
                [ prev
                    |> List.map (\q -> H.li [] [viewLinkToQuestion q])
                    |> H.ul [HA.style "width" "50%"]
                , next
                    |> List.map (\q -> H.li [] [viewLinkToQuestion q])
                    |> H.ul [HA.style "width" "50%"]
                ]
            ]

viewLinkToQuestion : Zipper Node -> Html Msg
viewLinkToQuestion zipper =
    H.button [HE.onClick (Focus zipper)] [H.text (Z.label zipper).question.prompt]

viewActiveQuestion : IsolatedQuestion -> Html Msg
viewActiveQuestion question =
    let
        optionToShortcut : Dict Option String
        optionToShortcut =
            Dict.fromList <| List.map2 Tuple.pair
                question.allOptions
                (String.split "" "abcdefghijklmnopqrstuvwxyz")

        shortcutToOption : Dict String Option
        shortcutToOption =
            optionToShortcut
            |> Dict.toList
            |> List.map (\(o,s) -> (s,o))
            |> Dict.fromList

        onKeydown : String -> Msg
        onKeydown key =
            if (Debug.log "key" key) == "ArrowLeft" then
                Backward
            else if key == "ArrowRight" then
                Forward
            else
                Dict.get key shortcutToOption |> Maybe.map ToggleChecked |> Maybe.withDefault Ignore

    in
    H.div []
        [ H.text question.prompt
        , question.allOptions
          |> List.map (\option -> H.button
                [ HA.style "outline" (if Set.member option question.checked then "3px solid green" else "")
                , HE.onClick (ToggleChecked option)
                ]
                [ case Dict.get option optionToShortcut of
                    Nothing -> H.text ""
                    Just shortcut -> H.strong [] [ H.text <| "(" ++ shortcut ++ ") "]
                , H.text option
                ]
            )
          |> H.div []
        , H.input
            [ HE.on "keydown" (JD.map onKeydown <| JD.field "key" JD.string)
            , HA.value ""
            , HA.id "shortcut-input"
            , HA.placeholder "shortcuts or arrow keys..."
            ]
            []
        ]


main = Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
    }
