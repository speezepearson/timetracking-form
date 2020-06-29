{--
Implementation of the TagTime Universal Ping Schedule
Spec: https://forum.beeminder.com/t/official-reference-implementation-of-the-tagtime-universal-ping-schedule/4282
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
  { meanGap = 45*60         -- Average gap between pings, in seconds
  , lastPingUnixTime = 1184097393 -- Ur-ping ie the birth of Timepie/TagTime! (unixtime)
  , seed   = 11193462   -- Initial state of the random number generator
  }

-- Above URPING is in 2007 and it's fine to jump to any later URPING/SEED pair
-- like this one in 2018 -- URPING = 1532992625, SEED = 75570 -- without
-- deviating from the universal ping schedule.

ia = 16807          -- =7^5: Multiplier for LCG random number generator
im = 2147483647     -- =2^31-1: Modulus used for the RNG



fromTime : Time.Posix -> Pinger
fromTime time =
  stepUntil time urPinger

stepUntil : Time.Posix -> Pinger -> Pinger
stepUntil time pinger =
  let
    (next, steppedPinger) = nextPing pinger
  in
    if Time.posixToMillis next > Time.posixToMillis time then
      pinger
    else
      stepUntil time steppedPinger


type Pinger = Pinger
  { meanGap : Int
  , lastPingUnixTime : Int
  , seed : Int
  }

meanGap : Pinger -> Int
meanGap (Pinger internals) =
  internals.meanGap



stepLcg : Pinger -> (Int, Pinger)
stepLcg (Pinger internals) =
  let
    {seed} = internals
    newSeed = (ia * seed) |> modBy im
  in
    (newSeed, Pinger {internals | seed  = newSeed})

nextGap : Pinger -> (Int, Pinger)
nextGap pinger =
  let
    (uniformRand, newPinger) = stepLcg pinger
    expRand = -(meanGap pinger |> toFloat) * logBase e (toFloat uniformRand / toFloat im)
    gap = max 1 (round expRand)
  in
    (gap, newPinger)

nextPing : Pinger -> (Time.Posix, Pinger)
nextPing pinger =
  let
    (gap, Pinger internalsWithNewLcg) = nextGap pinger
    resultUnixTime = internalsWithNewLcg.lastPingUnixTime + gap
  in
    ( Time.millisToPosix (resultUnixTime*1000)
    , Pinger { internalsWithNewLcg | lastPingUnixTime = resultUnixTime }
    )


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


main = Browser.sandbox { init=init, view=view, update=update }
init : Pinger
init = urPinger
view : Pinger -> Html.Html ()
view pinger = Html.div [] [Html.button [Html.Events.onClick ()] [Html.text "step"], Html.text (Debug.toString pinger)]
update : () -> Pinger -> Pinger
update _ pinger = Tuple.second (nextPing pinger)
