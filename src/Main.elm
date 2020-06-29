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
import Http

import Dict exposing (Dict)
import List.Extra
import Set exposing (Set)
import Tree as T exposing (Tree)
import Tree.Zipper as Z exposing (Zipper)

import Protobuf.Decode
import Protobuf.Encode

import Endpoints
import Proto

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

nodeTreeToProtobuf : Tree Node -> Proto.NodeTree
nodeTreeToProtobuf tree =
  { root = T.label tree |> nodeToProtobuf |> Just
  , children = T.children tree |> List.map nodeTreeToProtobuf |> Proto.NodeTreeChildren
  }
nodeTreeFromProtobuf : Proto.NodeTree -> Result String (Tree Node)
nodeTreeFromProtobuf {root, children} =
  case (root, children) of
    (Nothing, _) -> Err "no root"
    (Just pblabel, Proto.NodeTreeChildren pbchildren) ->
      nodeFromProtobuf pblabel |> Result.map (\label ->
        T.tree label
        (pbchildren |> List.map nodeTreeFromProtobuf |> List.concatMap (\r ->
          case r of
            Ok x -> [x]
            Err e -> Debug.log ("error parsing protobuf: " ++ Debug.toString e) []
          ))
        )

nodeToProtobuf : Node -> Proto.Node
nodeToProtobuf node =
  { prompt = prompt node
  , notes = notes node
  , kind = Just <| case node of
      SelectManyNode data ->
        Proto.KindSelectMany
          { allOptions = data.allOptions
          , checked = data.checked |> Maybe.map (Set.toList >> List.sort >> Proto.Strings)
          , ifCheckedFollowups = data.ifCheckedFollowups |> Dict.map (\_ v -> v |> Set.toList |> List.sort |> Proto.Strings |> Just)
          , ifUncheckedFollowups = data.ifUncheckedFollowups |> Dict.map (\_ v -> v |> Set.toList |> List.sort |> Proto.Strings |> Just)
          }
      SelectOneNode data ->
        Proto.KindSelectOne
          { allOptions = data.allOptions
          , selected = data.selected |> Maybe.map Proto.MaybeString
          , followups = data.followups |> Dict.map (\_ v -> v |> Set.toList |> List.sort |> Proto.Strings |> Just)
          }
  }
nodeFromProtobuf : Proto.Node -> Result String Node
nodeFromProtobuf pbnode =
  case pbnode.kind of
    Just (Proto.KindSelectMany data) -> Ok <|
      SelectManyNode
        { prompt = pbnode.prompt
        , allOptions = data.allOptions
        , checked = data.checked |> Maybe.map (.values >> Set.fromList)
        , ifCheckedFollowups = data.ifCheckedFollowups |> Dict.map (\_ -> Maybe.map (.values >> Set.fromList) >> Maybe.withDefault Set.empty)
        , ifUncheckedFollowups = data.ifUncheckedFollowups |> Dict.map (\_ -> Maybe.map (.values >> Set.fromList) >> Maybe.withDefault Set.empty)
        , notes = pbnode.notes
        , addOptionField = ""
        }
    Just (Proto.KindSelectOne data) -> Ok <|
      SelectOneNode
        { prompt = pbnode.prompt
        , allOptions = data.allOptions
        , selected = data.selected |> Maybe.map .value
        , followups = data.followups |> Dict.map (\_ -> Maybe.map (.values >> Set.fromList) >> Maybe.withDefault Set.empty)
        , notes = pbnode.notes
        , addOptionField = ""
        }
    kind -> Err ("unknown node-kind: " ++ Debug.toString kind)

prompt : Node -> Prompt
prompt node =
  case node of
    SelectManyNode data -> data.prompt
    SelectOneNode data -> data.prompt

notes : Node -> Prompt
notes node =
  case node of
    SelectManyNode data -> data.notes
    SelectOneNode data -> data.notes

isAnswered : Node -> Bool
isAnswered node =
  case node of
    SelectManyNode data -> (data.checked /= Nothing)
    SelectOneNode data -> (data.selected /= Nothing)

obliterateAnswer : Node -> Node
obliterateAnswer node =
  case node of
    SelectManyNode data -> SelectManyNode {data | checked = Nothing}
    SelectOneNode data -> SelectOneNode {data | selected = Nothing}

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
      -- |> Debug.log (prompt node ++ " / selected")
      |> Maybe.andThen (\s -> Dict.get s followups)
      |> Maybe.withDefault Set.empty
      -- |> Debug.log ("...has followups")
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
    , isCommitted : Bool
    }

type Msg
    = ToggleChecked Option
    | Select Option
    | SetAddOptionField String
    | AddOption
    | Focus (Zipper Node)
    | Forward
    | Backward
    | GotLastPing (Result Http.Error Proto.GetLastPingResponse)
    | CommitPing
    | PingCommitted (Result Http.Error Proto.WritePingResponse)
    | Ignore

init : () -> ( Model , Cmd Msg )
init () =
  ( { focus = Z.fromTree myTree
    , isCommitted = False
    }
  , Cmd.batch
    [ focusOnShortcuts
    , Endpoints.getLastPing GotLastPing Proto.GetLastPingRequest
    ]
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
              , isCommitted = False
              }
            , focusOnShortcuts
            )
        Select option ->
            ( { model
              | focus = model.focus |> Z.mapLabel (\node -> case node of
                  SelectOneNode data -> SelectOneNode { data | selected = Just option }
                  x -> x |> Debug.log ("ignoring " ++ Debug.toString msg ++ " for non-SelectOneNode")
                )
              , isCommitted = False
              }
            , Cmd.none
            )
            |> Tuple.mapFirst (update Forward >> Tuple.first)
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
              , isCommitted = False
              }
            , focusOnShortcuts
            )
        Focus focus ->
            ( { model | focus = focus }
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
        GotLastPing (Ok response) ->
            case response.lastPing
                 |> Debug.log "got last ping"
                 |> Maybe.andThen .answers
                 |> Result.fromMaybe "unpopulated protobuf"
                 |> Result.andThen nodeTreeFromProtobuf of
              Ok tree ->
                ( { model | focus = tree |> T.map obliterateAnswer |> Z.fromTree
                  }
                , Cmd.none
                )
              Err e ->
                Debug.log ("error parsing server response for last ping: " ++ Debug.toString e) <|
                ( model
                , Cmd.none
                )
        GotLastPing (Err e) ->
            Debug.log ("error loading last ping: " ++ Debug.toString e) <|
            ( model
            , Cmd.none
            )
        CommitPing ->
            ( model
            , Endpoints.writePing PingCommitted
              <| (\pbtree -> Proto.WritePingRequest (Just {unixTime = 123 , answers = Just pbtree}))
              <| nodeTreeToProtobuf
              <| Z.toTree model.focus
            )
        PingCommitted (Ok _) ->
            ( { model | isCommitted = True }
            , Cmd.none
            )
        PingCommitted (Err e) ->
            Debug.log ("error committing ping: " ++ Debug.toString e) <|
            ( model
            , Cmd.none
            )
        Ignore ->
            ( model , Cmd.none )

isFocusRelevant : Zipper Node -> Bool
isFocusRelevant zipper =
    case Z.parent zipper of
        Nothing -> True
        Just parent ->
          isFollowupRelevant (Z.label parent) (prompt <| Z.label zipper) && isFocusRelevant parent

nextRelevant : Zipper Node -> Maybe (Zipper Node)
nextRelevant zipper =
    -- Debug.log ("next relevant for " ++ prompt (Z.label zipper)) <|
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
    -- Debug.log ("relevant successors of " ++ prompt (Z.label zipper)) <|
    case nextRelevant zipper of
        Nothing -> []
        Just next -> next :: relevantSuccessors next

view : Model -> Html Msg
view {focus, isCommitted} =
    let
        -- _ = Debug.log "focus" (Z.label focus)
        -- _ = Debug.log "forward" (Z.forward focus |> Maybe.map Z.label)
        allPosns : List (Zipper Node)
        allPosns =
          let
            successors : Zipper Node -> List (Zipper Node)
            successors z = z :: (Z.forward z |> Maybe.map successors |> Maybe.withDefault [])
          in
            successors (Z.root focus)

        done = allPosns |> List.filter (\z -> isFocusRelevant z && isAnswered (Z.label z))
        todo = allPosns |> List.filter (\z -> isFocusRelevant z && not (isAnswered (Z.label z)))
        -- _ = Debug.log "next" (List.map (Z.label >> prompt) next)
    in
        H.div []
            [ H.button
                [ HE.onClick CommitPing
                , HA.disabled <| isCommitted || (List.any (not << isAnswered) <| T.flatten <| pruneIrrelevant <| Z.toTree focus)
                ]
                [ H.text "Commit" ]
            , viewActiveQuestion (Z.label focus)
            , H.hr [] []
            , H.div [HA.style "display" "flex"]
                [ H.div [HA.style "width" "50%"]
                    [ H.text "Answered:"
                    , done
                      |> List.map (\q -> H.li [] [viewLinkToQuestion q])
                      |> H.ul []
                    ]
                , H.div [HA.style "width" "50%"]
                    [ H.text "Todo:"
                    , todo
                      |> List.map (\q -> H.li [] [viewLinkToQuestion q])
                      |> H.ul []
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
        Nothing -> ""
        Just checkedSet -> checkedSet |> Set.toList |> List.sort |> String.join ", "
    SelectOneNode data ->
      case data.selected of
        Nothing -> ""
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

        onKeydown : { shift : Bool , ctrl : Bool , key : String } -> Msg
        onKeydown {shift, ctrl, key} =
          case (shift, ctrl, key) of
            (True, False, "Enter") ->
                Backward
            (_, _, "ArrowLeft") ->
                Backward
            (False, False, "Enter") ->
                Forward
            (False, True, "Enter") ->
                CommitPing
            (_, _, "ArrowRight") ->
                Forward
            (False, False, _) ->
                case String.uncons key of
                  Just (char, "") ->
                    Dict.get char shortcutToOption
                    |> Maybe.map ToggleChecked
                    |> Maybe.withDefault Ignore
                  _ -> Ignore
            (_, _, _) ->
                Ignore

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
            [ HE.on "keydown" (JD.map3 (\shift ctrl key -> onKeydown {shift=shift,ctrl=ctrl,key=key}) (JD.field "shiftKey" JD.bool) (JD.field "ctrlKey" JD.bool) (JD.field "key" JD.string))
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

        onKeydown : { shift : Bool , ctrl : Bool , key : String } -> Msg
        onKeydown {shift, ctrl, key} =
          case (shift, ctrl, key) of
            (True, False, "Enter") ->
                Backward
            (_, _, "ArrowLeft") ->
                Backward
            (False, False, "Enter") ->
                Forward
            (False, True, "Enter") ->
                CommitPing
            (_, _, "ArrowRight") ->
                Forward
            (False, False, _) ->
                case String.uncons key of
                  Just (char, "") ->
                    Dict.get char shortcutToOption
                    |> Maybe.map Select
                    |> Maybe.withDefault Ignore
                  _ -> Ignore
            (_, _, _) ->
                Ignore

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
            [ HE.on "keydown" (JD.map3 (\shift ctrl key -> onKeydown {shift=shift,ctrl=ctrl,key=key}) (JD.field "shiftKey" JD.bool) (JD.field "ctrlKey" JD.bool) (JD.field "key" JD.string))
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
  T.tree areYouAwake
    [ T.singleton whoAreYouWith
    , T.tree areYouDoingYourJob
      [ T.singleton whatJobStuffAreYouDoing
      ]
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
    , followups = Dict.singleton "yes" <| Set.fromList [prompt whoAreYouWith, prompt areYouDoingYourJob]
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
    , followups = Dict.singleton "yes" <| Set.fromList [prompt whatJobStuffAreYouDoing]
    }

whatJobStuffAreYouDoing : Node
whatJobStuffAreYouDoing =
  SelectManyNode
    { prompt = "What job-stuff are you doing?"
    , allOptions = ["meeting", "coding", "learning"]
    , checked = Nothing
    , addOptionField = ""
    , notes = ""
    , ifCheckedFollowups = Dict.empty
    , ifUncheckedFollowups = Dict.empty
    }
