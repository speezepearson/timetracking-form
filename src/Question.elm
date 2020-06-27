module Question exposing
    ( ..
    )

import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE

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

init : Model
init =
    { focus = Z.fromTree <| T.tree
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

update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleChecked option ->
            { model | focus = model.focus |> Z.mapLabel (\n -> {n | question = n.question |> toggle option}) }
            |> (\m -> Debug.log ("got ToggleChecked; qt is " ++ Debug.toString (toQuestionTree (Z.toTree m.focus))) m)
        Focus focus ->
            { model | focus = focus |> Debug.log ("focusing on " ++ Debug.toString (Z.label focus)) }

iterateUntilNothing : (a -> Maybe a) -> Maybe a -> List a
iterateUntilNothing f mx =
    case mx of
        Nothing -> []
        Just x -> x :: iterateUntilNothing f (f x)

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

relevantPredecessors : Zipper Node -> List (Zipper Node)
relevantPredecessors zipper =
    iterateUntilNothing
        Z.backward
        (Z.backward zipper)
    |> List.filter isRelevant

relevantSuccessors : Zipper Node -> List (Zipper Node)
relevantSuccessors zipper =
    iterateUntilNothing
        Z.forward
        (Z.forward zipper)
    |> List.filter isRelevant

view : Model -> Html Msg
view {focus} =
    let
        root : Form
        root = Z.tree (Z.root focus)

        _ = Debug.log "focus" (Z.label focus)
        _ = Debug.log "forward" (Z.forward focus |> Maybe.map Z.label)

        prev = relevantPredecessors focus
        next = relevantSuccessors focus
        _ = Debug.log "next" (List.map (Z.label >> .question >> .prompt) next)
    in
        H.div []
            [ viewActiveQuestion True (Z.label focus).question
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

viewActiveQuestion : Bool -> IsolatedQuestion -> Html Msg
viewActiveQuestion interactive question =
    H.div []
        [ H.text question.prompt
        , question.allOptions
          |> List.map (\option -> H.button
                [ HA.style "outline" (if Set.member option question.checked then "3px solid green" else "")
                , HE.onClick (ToggleChecked option)
                , HA.disabled (not interactive)
                ]
                [H.text option]
            )
          |> H.div []
        ]


main = Browser.sandbox
    { init = init
    , view = view
    , update = update
    }
