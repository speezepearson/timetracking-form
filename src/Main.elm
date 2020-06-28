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
import Task

import Dict exposing (Dict)
import List.Extra
import Set exposing (Set)
import Tree as T exposing (Tree)
import Tree.Zipper as Z exposing (Zipper)

type alias Prompt = String
type alias Option = String


type Node
  = SelectManyNode
    { prompt : Prompt
    , allOptions : List Option
    , checked : Maybe (Set Option)
    , ifCheckedFollowups : Dict Option (Set Prompt)
    , ifUncheckedFollowups : Dict Option (Set Prompt)
    , addOptionField : String
    , notes : String
    }
  | SelectOneNode
    { prompt : Prompt
    , allOptions : List Option
    , selected : Maybe Option
    , followups : Dict Option (Set Prompt)
    , addOptionField : String
    , notes : String
    }

-- TODO: add serialization tests

encodeNode : Node -> JE.Value
encodeNode node =
  case node of
    SelectManyNode data ->
      JE.object
        [ ( "type", "SelectManyNode" |> JE.string )
        , ( "prompt", data.prompt |> JE.string  )
        , ( "allOptions", data.allOptions |> JE.list JE.string )
        , ( "checked", data.checked |> Maybe.map (Set.toList >> JE.list JE.string) |> Maybe.withDefault JE.null )
        , ( "ifCheckedFollowups", data.ifCheckedFollowups |> JE.dict identity (Set.toList >> JE.list JE.string) )
        , ( "ifUncheckedFollowups", data.ifUncheckedFollowups |> JE.dict identity (Set.toList >> JE.list JE.string) )
        -- , ( "addOptionField", data.addOptionField )
        , ( "notes", data.notes |> JE.string )
        ]
    SelectOneNode data ->
      JE.object
        [ ( "type", "SelectOneNode" |> JE.string )
        , ( "prompt", data.prompt |> JE.string  )
        , ( "allOptions", data.allOptions |> JE.list JE.string )
        , ( "selected", data.selected |> Maybe.map JE.string |> Maybe.withDefault JE.null )
        , ( "followups", data.followups |> JE.dict identity (Set.toList >> JE.list JE.string) )
        -- , ( "addOptionField", data.addOptionField )
        , ( "notes", data.notes |> JE.string )
        ]

nodeDecoder : JD.Decoder Node
nodeDecoder =
  JD.field "type" JD.string |> JD.andThen (\type_ ->
    if type_ == "SelectManyNode" then
      JD.map6 (\prompt_ allOptions checked ifCheckedFollowups ifUncheckedFollowups notes -> SelectManyNode {prompt=prompt_, allOptions=allOptions, checked=checked, notes=notes, ifCheckedFollowups=ifCheckedFollowups, ifUncheckedFollowups=ifUncheckedFollowups, addOptionField=""})
        (JD.field "prompt" JD.string)
        (JD.field "allOptions" <| JD.list JD.string)
        (JD.field "checked" <| (JD.nullable (JD.list JD.string) |> JD.map (Maybe.map Set.fromList)))
        (JD.field "ifCheckedFollowups" <| JD.dict (JD.list JD.string |> JD.map Set.fromList))
        (JD.field "ifUncheckedFollowups" <| JD.dict (JD.list JD.string |> JD.map Set.fromList))
        (JD.field "notes" JD.string)
    else if type_ == "SelectOneNode" then
      JD.map5 (\prompt_ allOptions selected followups notes -> SelectOneNode {prompt=prompt_, allOptions=allOptions, selected=selected, notes=notes, followups=followups, addOptionField=""})
        (JD.field "prompt" JD.string)
        (JD.field "allOptions" <| JD.list JD.string)
        (JD.field "selected" <| JD.nullable JD.string)
        (JD.field "followups" <| JD.dict (JD.list JD.string |> JD.map Set.fromList))
        (JD.field "notes" JD.string)
    else
      JD.fail ("unrecognized type: " ++ type_)
    )

prompt : Node -> Prompt
prompt node =
  case node of
    SelectManyNode data -> data.prompt
    SelectOneNode data -> data.prompt

isAnswered : Node -> Bool
isAnswered node =
  case node of
    SelectManyNode data -> (data.checked /= Nothing)
    SelectOneNode data -> (data.selected /= Nothing)

isFollowupRelevant : Node -> Prompt -> Bool
isFollowupRelevant node followupPrompt =
  case node of
    SelectManyNode {allOptions, checked, ifCheckedFollowups, ifUncheckedFollowups} ->
      case checked of
        Nothing -> False
        Just checkedSet ->
          allOptions
          |> List.map (\option ->
              (if Set.member option checkedSet then ifCheckedFollowups else ifUncheckedFollowups)
              |> Dict.get option
              |> Maybe.withDefault Set.empty
            )
          |> List.foldl Set.union Set.empty
          |> Set.member followupPrompt
    SelectOneNode {selected, followups} ->
      selected
      |> Maybe.andThen (\s -> Dict.get s followups)
      |> Maybe.withDefault Set.empty
      |> Set.member followupPrompt


pruneIrrelevant : Tree Node -> Tree Node
pruneIrrelevant tree =
  let
    root = T.label tree
  in
    tree |> T.mapChildren (
      List.filter (T.label >> prompt >> isFollowupRelevant root)
      >> List.map pruneIrrelevant
    )


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
                      { data | checked = data.checked |> Maybe.withDefault Set.empty |> (\checked -> if Set.member option checked then Set.remove option checked else Set.insert option checked) |> Just }
                  x -> x |> Debug.log ("ignoring " ++ Debug.toString msg ++ " for non-SelectManyNode")
                )
              }
            , Cmd.none
            )
        Select option ->
            ( { model
              | focus = model.focus |> Z.mapLabel (\node -> case node of
                  SelectOneNode data -> SelectOneNode { data | selected = Just option }
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
                  SelectManyNode data -> SelectManyNode { data | allOptions = data.allOptions ++ [data.addOptionField] , checked = data.checked |> Maybe.withDefault Set.empty |> Set.insert data.addOptionField |> Just , addOptionField = "" }
                  SelectOneNode data -> SelectOneNode { data | allOptions = data.allOptions ++ [data.addOptionField] , selected = Just data.addOptionField , addOptionField = "" }
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
    H.span []
      [ H.button [HE.onClick (Focus zipper)] [H.text <| prompt <| Z.label zipper]
      , H.text <| " " ++ summarizeAnswer (Z.label zipper)
      ]

summarizeAnswer : Node -> String
summarizeAnswer node =
  case node of
    SelectManyNode data ->
      case data.checked of
        Nothing -> "(TODO)"
        Just checkedSet -> checkedSet |> Set.toList |> List.sort |> String.join ", "
    SelectOneNode data ->
      case data.selected of
        Nothing -> "(TODO)"
        Just selectedStr -> selectedStr

shortcuts : List String -> Dict String Char
shortcuts =
  let
    winnersAtIndex : Set Char -> Int -> List String -> Dict String Char
    winnersAtIndex taken index strs =
      case strs of
        [] -> Dict.empty
        s :: rest ->
          case String.uncons (String.dropLeft index <| String.toLower s) of
            Just (c, _) ->
              if Set.member c taken then
                winnersAtIndex taken index rest
              else
                ( Dict.insert s c <|
                  winnersAtIndex (Set.insert c taken) index rest
                )
            Nothing ->
              winnersAtIndex taken index rest

    shortcutsWithExclusions : Set Char -> Int -> List String -> Dict String Char
    shortcutsWithExclusions taken startAt strs =
      let
        immediateWinners = winnersAtIndex taken startAt strs
        nonHopelessLosers =
          strs
          |> List.filter (\s -> not (Dict.member s immediateWinners))
          |>  List.filter (\s -> String.length s > startAt)
        _ = Debug.log "state" ((taken, startAt, strs), (immediateWinners, nonHopelessLosers))
      in
        if List.isEmpty nonHopelessLosers then
          immediateWinners
        else
          Dict.union immediateWinners <|
          shortcutsWithExclusions
            (Set.union taken <| Set.fromList <| Dict.values immediateWinners)
            (startAt+1)
            nonHopelessLosers
  in
    shortcutsWithExclusions Set.empty 0

emphasizeShortcut : Char -> String -> Html msg
emphasizeShortcut shortcutChar str =
  let shortcut = String.fromChar shortcutChar in
  case String.indexes shortcut (String.toLower str) of
    [] ->
      H.span []
        [ H.strong [] [H.text <| "("++shortcut++")"]
        , H.text <| " " ++ str
        ]
    i :: _ ->
      H.span []
        [ H.text (String.left i str)
        , H.strong [] [H.text <| "("++shortcut++")"]
        , H.text (String.dropLeft (i+1) str)
        ]

viewActiveQuestion : Node -> Html Msg
viewActiveQuestion node =
  case node of
    SelectManyNode data ->
      let
        optionToShortcut : Dict Option Char
        optionToShortcut =
          shortcuts (data.allOptions)

        shortcutToOption : Dict Char Option
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
                case String.uncons key of
                  Just (char, "") -> Dict.get char shortcutToOption |> Maybe.map ToggleChecked |> Maybe.withDefault Ignore
                  _ -> Ignore

      in
      H.div []
        [ H.text (prompt node)
        , data.allOptions
          |> List.map (\option -> H.button
                [ HA.style "outline" (if Set.member option (data.checked |> Maybe.withDefault Set.empty) then "3px solid green" else "")
                , HE.onClick (ToggleChecked option)
                ]
                [ case Dict.get option optionToShortcut of
                    Nothing -> H.text option
                    Just shortcut -> emphasizeShortcut shortcut option
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

    SelectOneNode data ->
      let
        optionToShortcut : Dict Option Char
        optionToShortcut =
          shortcuts (data.allOptions)

        shortcutToOption : Dict Char Option
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
                case String.uncons key of
                  Just (char, "") -> Dict.get char shortcutToOption |> Maybe.map Select |> Maybe.withDefault Ignore
                  _ -> Ignore

      in
      H.div []
        [ H.text (prompt node)
        , data.allOptions
          |> List.map (\option -> H.button
                [ HA.style "outline" (if Just option == data.selected then "3px solid green" else "")
                , HE.onClick (Select option)
                ]
                [ case Dict.get option optionToShortcut of
                    Nothing -> H.text option
                    Just shortcut -> emphasizeShortcut shortcut option
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
    areYouAwake
    [ T.singleton whoAreYouWith
    , T.singleton areYouDoingYourJob
    ]

containsPrompt : Set comparable -> comparable -> Bool
containsPrompt s x = Set.member x  s

areYouAwake : Node
areYouAwake =
  SelectOneNode
    { prompt = "Are you awake?"
    , allOptions = ["yes", "no"]
    , selected = Nothing
    , addOptionField = ""
    , notes = ""
    , followups = Dict.fromList [("yes", Set.fromList [prompt whoAreYouWith, prompt areYouDoingYourJob])]
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
    , checked = Nothing
    , addOptionField = ""
    , notes = ""
    , ifCheckedFollowups = Dict.empty
    , ifUncheckedFollowups = Dict.empty
    }

areYouDoingYourJob : Node
areYouDoingYourJob =
  SelectOneNode
    { prompt = "Are you doing your job?"
    , allOptions = ["yes", "no"]
    , selected = Nothing
    , addOptionField = ""
    , notes = ""
    , followups = Dict.empty
    }
