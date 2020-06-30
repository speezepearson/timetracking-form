module TagTime.Demo exposing (..)

import Browser
import Html
import Html.Attributes
import Html.Events
import Task
import Time

import TagTime exposing (Pinger, isAfter)

type alias Model =
  { now : Time.Posix
  , startingAt : Time.Posix
  , timeZone : Time.Zone
  , pings : List Time.Posix
  , pinger : Pinger
  }

type Msg
  = Tick Time.Posix
  | SetZone Time.Zone
  | Step

main = Browser.element
  { init = init
  , view = view
  , update = update
  , subscriptions = (\_ -> Time.every 1000 Tick)
  }

init : () -> ( Model , Cmd Msg )
init () =
  ( { now = Time.millisToPosix 0
    , startingAt = Time.millisToPosix 0
    , timeZone = Time.utc
    , pings = []
    , pinger = TagTime.urPinger
    }
  , Cmd.batch
      [ Task.perform Tick Time.now
      , Task.perform SetZone Time.here
      ]
  )

view : Model -> Html.Html Msg
view {startingAt, pings, pinger, now, timeZone} =
  let
    atLimit = Tuple.first (TagTime.nextPing pinger) |> isAfter now
  in
    Html.div []
      [ Html.text <| "Pings since " ++ localTimeString timeZone startingAt ++ ": "
      , Html.button
          [ Html.Events.onClick Step
          , Html.Attributes.disabled atLimit
          ]
          [ if atLimit then
              Html.text "(refusing to go past current time...)"
            else
              Html.text "(more)"
          ]
      , pings
        |> List.map (localTimeString timeZone >> Html.text >> List.singleton >> Html.li [])
        |> Html.ul []
      ]

localTimeString : Time.Zone -> Time.Posix -> String
localTimeString zone time =
  (String.fromInt <| Time.toYear zone time)
  ++ " " ++ (Debug.toString <| Time.toMonth zone time)
  ++ " " ++ (String.pad 2 '0' <| String.fromInt <| Time.toDay zone time)
  ++ " " ++ (String.pad 2 '0' <| String.fromInt <| Time.toHour zone time)
  ++ ":" ++ (String.pad 2 '0' <| String.fromInt <| Time.toMinute zone time)
  ++ ":" ++ (String.pad 2 '0' <| String.fromInt <| Time.toSecond zone time)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Tick now ->
      ( if Time.posixToMillis (model.now) == 0 then
          let
            startingAt = Time.millisToPosix <| (\x -> x-1000*60*60*5) <| Time.posixToMillis now
          in
            { model
            | now = now
            , pinger = TagTime.fromTime startingAt
            , startingAt = startingAt
            }
        else
          { model | now = now }
      , Cmd.none
      )
    SetZone zone ->
      ( { model | timeZone = zone }
      , Cmd.none
      )
    Step ->
      ( let
          (p, np) = TagTime.nextPing model.pinger
        in
          if p |> isAfter model.now then
            model
          else
            { model
            | pings = p :: model.pings
            , pinger = np
            }
      , Cmd.none
      )