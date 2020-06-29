{--
Implementation of the TagTime Universal Ping Schedule
Spec: https://forum.beeminder.com/t/official-reference-implementation-of-the-tagtime-universal-ping-schedule/4282

This module is written in a less-than-maximally Elm-like style in order to stay easily comparable to the reference implementation.

--}

module TagTime exposing
  ( Pinger
  , fromTime
  , nextPing
  , urPinger
  , meanGap
  , stepUntil
  )

import Browser
import Html
import Html.Events

import Time

urPinger : Pinger
urPinger = Pinger
  { meanGap          = 45*60        -- Average gap between pings, in seconds
  , lastPingUnixTime = 1184097393   -- Ur-ping ie the birth of Timepie/TagTime! (unixtime)
  , lcg              = Lcg 11193462 -- Initial state of the random number generator
  }

-- Above URPING is in 2007 and it's fine to jump to any later URPING/SEED pair
-- like this one in 2018 -- URPING = 1532992625, SEED = 75570 -- without
-- deviating from the universal ping schedule.

ia = 16807          -- =7^5: Multiplier for LCG random number generator
im = 2147483647     -- =2^31-1: Modulus used for the RNG

type Lcg = Lcg Int
stepLcg : Lcg -> (Int, Lcg)
stepLcg (Lcg seed) =
  let
    newSeed = (ia * seed) |> modBy im
  in
    (newSeed, Lcg newSeed)

expRand : Float -> Lcg -> (Float, Lcg)
expRand scale lcg =
  let
    (uniformRand, newLcg) = stepLcg lcg
    result = -scale * logBase e (toFloat uniformRand / toFloat im)
  in
    (result, newLcg)


type Pinger = Pinger
  { meanGap : Int
  , lastPingUnixTime : Int
  , lcg : Lcg
  }


nextPing : Pinger -> (Time.Posix, Pinger)
nextPing pinger =
  let
    (Pinger internals) = pinger
    (eRand, newLcg) = expRand (meanGap pinger |> toFloat) internals.lcg
    gap = max 1 (round eRand)
    resultUnixTime = internals.lastPingUnixTime + gap
  in
    ( Time.millisToPosix (resultUnixTime*1000)
    , Pinger { internals | lcg = newLcg , lastPingUnixTime = resultUnixTime }
    )


-- equivalent of init() in the reference implementation
stepUntil : Time.Posix -> Pinger -> Pinger
stepUntil time pinger =
  let
    (next, steppedPinger) = nextPing pinger
  in
    if Time.posixToMillis next > Time.posixToMillis time then
      pinger
    else
      stepUntil time steppedPinger



-- EXTRA FUNCTIONS FOR MANIPULATING PINGERS

lastPing : Pinger -> Time.Posix
lastPing (Pinger {lastPingUnixTime}) =
  Time.millisToPosix (lastPingUnixTime*1000)

meanGap : Pinger -> Int
meanGap (Pinger internals) =
  internals.meanGap

fromTime : Time.Posix -> Pinger
fromTime time =
  stepUntil time urPinger






-- DEBUGGING STUFF THAT I SHOULD PACKAGE INTO TESTS

firstFewPings : List (Time.Posix)
firstFewPings =
  let
    p0 = urPinger
    (t1, p1) = nextPing p0
    (t2, p2) = nextPing p1
    (t3, p3) = nextPing p2
    (t4, p4) = nextPing p3
    (t5, p5) = nextPing p4
  in
    [t1, t2, t3, t4, t5]

{-- TODO: tests

List.Extra.unfoldr
  (\p -> if (nextPing p |> Tuple.first |> Time.posixToMillis) > 1184105815000 then Nothing else Just (nextPing p))
  urPinger
== List.map (\sec -> Time.millisToPosix (sec*1000))
    [ 1184098754
    , 1184102685
    , 1184104776
    , 1184105302
    , 1184105815
    ]

Tuple.first (nextPing <| stepUntil <| Time.millisToPosix 1184104776000) == Time.millisToPosix 1184105302000

--}

type alias Model = { pings : List Time.Posix, pinger : Pinger }

main = Browser.sandbox { init=init, view=view, update=update }

init : Model
init = {pings=[], pinger=urPinger}

view : Model -> Html.Html ()
view {pings, pinger} = Html.div []
  [ Html.button [Html.Events.onClick ()] [Html.text "step"]
  , Html.text (Debug.toString pinger)
  , Html.br [] []
  , Html.text "Past pings:"
  , pings
    |> List.map (Time.posixToMillis >> (\x -> x//1000) >> String.fromInt >> Html.text >> List.singleton >> Html.li [])
    |> Html.ul []
  ]

update : () -> Model -> Model
update () model =
  let (p, pr) = nextPing model.pinger in
  { model
  | pings = p :: model.pings
  , pinger = pr
  }
