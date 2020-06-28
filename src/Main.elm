module Main exposing
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


type Node
  = SelectManyNode
    { prompt : Prompt
    , allOptions : List Option
    , checked : Set Option
    , relevantFollowups : Set Option -> Set Prompt
    , addOptionField : String
    }
  | SelectOneNode
    { prompt : Prompt
    , allOptions : List Option
    , selected : Option
    , relevantFollowups : Option -> Set Prompt
    , addOptionField : String
    }

prompt : Node -> Prompt
prompt node =
  case node of
    SelectManyNode data -> data.prompt
    SelectOneNode data -> data.prompt

isFollowupRelevant : Node -> Prompt -> Bool
isFollowupRelevant node followupPrompt =
  case node of
    SelectManyNode {checked, relevantFollowups} -> Set.member followupPrompt (relevantFollowups checked)
    SelectOneNode {selected, relevantFollowups} -> Set.member followupPrompt (relevantFollowups selected)


type alias Model =
    { focus : Zipper Node
    }

type Msg
    = ToggleChecked Option
    | Select Option
    | SetAddOptionField String
    | AddOption
    | Focus (Zipper Node)
    | Forward
    | Backward
    | Ignore

init : () -> ( Model , Cmd Msg )
init () =
  ( { focus = Z.fromTree myTree }
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
        ToggleChecked option ->
            ( { model
              | focus = model.focus |> Z.mapLabel (\node -> case node of
                  SelectManyNode data -> SelectManyNode
                      { data | checked = data.checked |> if Set.member option data.checked then Set.remove option else Set.insert option }
                  x -> x |> Debug.log ("ignoring " ++ Debug.toString msg ++ " for non-SelectManyNode")
                )
              }
            , Cmd.none
            )
        Select option ->
            ( { model
              | focus = model.focus |> Z.mapLabel (\node -> case node of
                  SelectOneNode data -> SelectOneNode { data | selected = option }
                  x -> x |> Debug.log ("ignoring " ++ Debug.toString msg ++ " for non-SelectOneNode")
                )
              }
            , Cmd.none
            )
        SetAddOptionField value ->
            ( { model
              | focus = model.focus |> Z.mapLabel (\node -> case node of
                  SelectManyNode data -> SelectManyNode { data | addOptionField = value }
                  SelectOneNode data -> SelectOneNode { data | addOptionField = value }
                  -- x -> x |> Debug.log ("ignoring " ++ Debug.toString msg ++ " for non-Select*Node")
                )
              }
            , Cmd.none
            )
        AddOption ->
            ( { model
              | focus = model.focus |> Z.mapLabel (\node -> case node of
                  SelectManyNode data -> SelectManyNode { data | allOptions = data.allOptions ++ [data.addOptionField] , checked = data.checked |> Set.insert data.addOptionField , addOptionField = "" }
                  SelectOneNode data -> SelectOneNode { data | allOptions = data.allOptions ++ [data.addOptionField] , selected = data.addOptionField , addOptionField = "" }
                  -- x -> x |> Debug.log ("ignoring " ++ Debug.toString msg ++ " for non-Select*Node")
                )
              }
            , focusOnShortcuts
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

isFocusRelevant : Zipper Node -> Bool
isFocusRelevant zipper =
    Debug.log ("is " ++ Debug.toString (Z.label zipper) ++ " relevant?") <|
    case Z.parent zipper of
        Nothing -> True
        Just parent ->
          isFollowupRelevant (Z.label parent) (prompt <| Z.label zipper) && isFocusRelevant parent

nextRelevant : Zipper Node -> Maybe (Zipper Node)
nextRelevant zipper =
    case Z.forward zipper of
        Nothing -> Nothing
        Just next ->
            if isFocusRelevant next then
                Just next
            else
                nextRelevant next

prevRelevant : Zipper Node -> Maybe (Zipper Node)
prevRelevant zipper =
    case Z.backward zipper of
        Nothing -> Nothing
        Just next ->
            if isFocusRelevant next then
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
        _ = Debug.log "next" (List.map (Z.label >> prompt) next)
    in
        H.div []
            [ viewActiveQuestion (Z.label focus)
            , H.hr [] []
            , H.div [HA.style "display" "flex"]
                [ H.div [HA.style "width" "50%"]
                    [ H.text "Previous questions:"
                    , prev
                      |> List.map (\q -> H.li [] [viewLinkToQuestion q])
                      |> H.ul [HA.style "width" "50%"]
                    ]
                , H.div [HA.style "width" "50%"]
                    [ H.text "Upcoming questions:"
                    , next
                      |> List.map (\q -> H.li [] [viewLinkToQuestion q])
                      |> H.ul [HA.style "width" "50%"]
                    ]
                ]
            ]

viewLinkToQuestion : Zipper Node -> Html Msg
viewLinkToQuestion zipper =
    H.button [HE.onClick (Focus zipper)] [H.text <| prompt <| Z.label zipper]

viewActiveQuestion : Node -> Html Msg
viewActiveQuestion node =
  case node of
    SelectManyNode data ->
      let
        optionToShortcut : Dict Option String
        optionToShortcut =
            Dict.fromList <| List.map2 Tuple.pair
                data.allOptions
                (String.split "" "abcdefghijklmnopqrstuvwxyz")

        shortcutToOption : Dict String Option
        shortcutToOption =
            optionToShortcut
            |> Dict.toList
            |> List.map (\(o,s) -> (s,o))
            |> Dict.fromList

        onKeydown : { shift : Bool , key : String } -> Msg
        onKeydown {shift, key} =
            if key == "ArrowLeft" || (shift && key == "Enter") then
                Backward
            else if key == "ArrowRight" || (not shift && key == "Enter") then
                Forward
            else
                Dict.get key shortcutToOption |> Maybe.map ToggleChecked |> Maybe.withDefault Ignore

      in
      H.div []
        [ H.text (prompt node)
        , data.allOptions
          |> List.map (\option -> H.button
                [ HA.style "outline" (if Set.member option data.checked then "3px solid green" else "")
                , HE.onClick (ToggleChecked option)
                ]
                [ case Dict.get option optionToShortcut of
                    Nothing -> H.text ""
                    Just shortcut -> H.strong [] [ H.text <| "(" ++ shortcut ++ ") "]
                , H.text option
                ]
            )
          |> (\hs -> hs ++
              [ H.input
                  [ HE.onInput SetAddOptionField
                  , HA.placeholder "add option"
                  , HA.value data.addOptionField
                  ]
                  []
              , H.button [HE.onClick AddOption] [H.text "Add"]
              ]
             )
          |> H.div []
        , H.input
            [ HE.on "keydown" (JD.map2 (\shift key -> onKeydown {shift=shift,key=key}) (JD.field "shiftKey" JD.bool) (JD.field "key" JD.string))
            , HA.value ""
            , HA.id "shortcut-input"
            , HA.placeholder "shortcuts or arrow keys..."
            ]
            []
        ]

    SelectOneNode data ->
      let
        optionToShortcut : Dict Option String
        optionToShortcut =
            Dict.fromList <| List.map2 Tuple.pair
                data.allOptions
                (String.split "" "abcdefghijklmnopqrstuvwxyz")

        shortcutToOption : Dict String Option
        shortcutToOption =
            optionToShortcut
            |> Dict.toList
            |> List.map (\(o,s) -> (s,o))
            |> Dict.fromList

        onKeydown : { shift : Bool , key : String } -> Msg
        onKeydown {shift, key} =
            if key == "ArrowLeft" || (shift && key == "Enter") then
                Backward
            else if key == "ArrowRight" || (not shift && key == "Enter") then
                Forward
            else
                Dict.get key shortcutToOption |> Maybe.map Select |> Maybe.withDefault Ignore

      in
      H.div []
        [ H.text (prompt node)
        , data.allOptions
          |> List.map (\option -> H.button
                [ HA.style "outline" (if option == data.selected then "3px solid green" else "")
                , HE.onClick (Select option)
                ]
                [ case Dict.get option optionToShortcut of
                    Nothing -> H.text ""
                    Just shortcut -> H.strong [] [ H.text <| "(" ++ shortcut ++ ") "]
                , H.text option
                ]
            )
          |> (\hs -> hs ++
              [ H.input
                  [ HE.onInput SetAddOptionField
                  , HA.placeholder "add option"
                  , HA.value data.addOptionField
                  , HE.on "keydown" (JD.map (\key -> if key == "Enter" then AddOption else Ignore) <| JD.field "key" JD.string)
                  ]
                  []
              , H.button [HE.onClick AddOption] [H.text "Add"]
              ]
             )
          |> H.div []
        , H.input
            [ HE.on "keydown" (JD.map2 (\shift key -> onKeydown {shift=shift,key=key}) (JD.field "shiftKey" JD.bool) (JD.field "key" JD.string))
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





-- MY TREE

myTree : Tree Node
myTree =
  T.tree
    areYouAsleep
    [ T.singleton whoAreYouWith
    , T.singleton areYouDoingYourJob
    ]

containsPrompt : Set comparable -> comparable -> Bool
containsPrompt s x = Set.member x  s

areYouAsleep : Node
areYouAsleep =
  SelectOneNode
    { prompt = "Are you asleep?"
    , allOptions = ["yes", "no"]
    , selected = "no"
    , addOptionField = ""
    , relevantFollowups = (\selected ->
        if selected == "yes" then
          Set.empty
        else
          Set.fromList [prompt whoAreYouWith, prompt areYouDoingYourJob]
      )
    }

whoAreYouWith : Node
whoAreYouWith =
  SelectManyNode
    { prompt = "Who are you with?"
    , allOptions = List.sort
      [ "Rebecca"
      , "Yam"
      , "Ellie"
      , "Crystal"
      , "Justin"
      , "Dad"
      , "Cathy"
      , "Florence"
      ]
    , checked = Set.empty
    , addOptionField = ""
    , relevantFollowups = always Set.empty
    }

areYouDoingYourJob : Node
areYouDoingYourJob =
  SelectOneNode
    { prompt = "Are you doing your job?"
    , allOptions = ["yes", "no"]
    , selected = "no"
    , addOptionField = ""
    , relevantFollowups = always Set.empty
    }
