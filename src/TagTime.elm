{--
Implementation of the TagTime Universal Ping Schedule
Spec: https://forum.beeminder.com/t/official-reference-implementation-of-the-tagtime-universal-ping-schedule/4282

This module is written in a less-than-maximally Elm-like style in order to stay easily comparable to the reference implementation.

--}

module TagTime exposing
  ( Ping
  , firstAfter
  , lastBefore
  , toTime
  , next
  , prev
  , urPing
  , meanGap
  , advanceToLastBefore
  , advanceToFirstAfter
  , isAfter
  , waitForPing
  )

import Process
import Task exposing (Task)
import Time

urPing : Ping
urPing = Ping
  { meanGap          = 45*60        -- Average gap between pings, in seconds
  , lastPingUnixTime = 1184097393   -- Ur-ping ie the birth of Timepie/TagTime! (unixtime)
  , lcg              = Lcg 11193462 -- Initial state of the random number generator
  }

-- Above URPING is in 2007 and it's fine to jump to any later URPING/SEED pair
-- like this one in 2018 -- URPING = 1532992625, SEED = 75570 -- without
-- deviating from the universal ping schedule.

ia : Int
ia = 16807          -- =7^5: Multiplier for LCG random number generator
im : Int
im = 2147483647     -- =2^31-1: Modulus used for the RNG

type Lcg = Lcg Int
stepLcg : Lcg -> (Int, Lcg)
stepLcg (Lcg seed) =
  let
    newSeed = mulModIm ia seed
  in
    (newSeed, Lcg newSeed)

expRand : Float -> Lcg -> (Float, Lcg)
expRand scale lcg =
  let
    (uniformRand, newLcg) = stepLcg lcg
    result = -scale * logBase e (toFloat uniformRand / toFloat im)
  in
    (result, newLcg)

nextGap : Int -> Lcg -> (Int, Lcg)
nextGap mean lcg =
  let
    (eRand, newLcg) = expRand (toFloat mean) lcg
  in
    (max 1 (round eRand), newLcg)


type Ping = Ping
  { meanGap : Int
  , lastPingUnixTime : Int
  , lcg : Lcg
  }

toTime : Ping -> Time.Posix
toTime (Ping {lastPingUnixTime}) =
  Time.millisToPosix <| lastPingUnixTime * 1000


next : Ping -> Ping
next ping =
  let
    (Ping internals) = ping
    (gap, newLcg) = nextGap internals.meanGap internals.lcg
    resultUnixTime = internals.lastPingUnixTime + gap
  in
    Ping { internals | lcg = newLcg , lastPingUnixTime = resultUnixTime }


-- GOING BACKWARDS

iaInv = 1407677000  -- Multiplicative inverse of ia; used to step backward.

retreat : Lcg -> Lcg
retreat (Lcg seed) =
  Lcg <| mulModIm seed iaInv

prev : Ping -> Ping
prev ping =
  let
    (Ping internals) = ping
    prevLcg = retreat internals.lcg
    (prevGap, _) = nextGap internals.meanGap prevLcg
    resultUnixTime = internals.lastPingUnixTime - prevGap
  in
    Ping { internals | lcg = prevLcg , lastPingUnixTime = resultUnixTime }

-- equivalent of init() in the reference implementation
advanceToLastBefore : Time.Posix -> Ping -> Ping
advanceToLastBefore time ping =
  let
    ping_ = next ping
  in
    if toTime ping_ |> isAfter time then
      ping
    else
      advanceToLastBefore time ping_

advanceToFirstAfter : Time.Posix -> Ping -> Ping
advanceToFirstAfter t p =
  advanceToLastBefore t p |> next

-- EXTRA FUNCTIONS FOR MANIPULATING PINGERS

meanGap : Ping -> Int
meanGap (Ping internals) =
  internals.meanGap

lastBefore : Time.Posix -> Ping
lastBefore time =
  advanceToLastBefore time urPing

firstAfter : Time.Posix -> Ping
firstAfter time =
  advanceToFirstAfter time urPing

pingsUntil : Time.Posix -> Ping -> List Ping
pingsUntil tf ping =
  let
    withAccumulator : List Ping -> Ping -> List Ping
    withAccumulator res ping_ =
      let
        np = next ping_
      in
        if toTime np |> isAfter tf then
          []
        else
          withAccumulator (np :: res) np
  in
    withAccumulator [] ping
    |> List.reverse



-- UTILITIES


-- Elm's Int arithmetic is quirky for numbers above 2^32.
-- This means we can't use normal multiplication for elements of the modular field:
--   we need a special algorithm that carefully avoids ever going outside the interval [-2^32, 2^32).
-- This implementation is adapted from Stack Overflow's inimitable "Matt":
-- https://stackoverflow.com/questions/21030153/modulo-of-multiplication-of-large-numbers/21032389#21032389
mulModIm : Int -> Int -> Int
mulModIm =
  let

    addModIm : Int -> Int -> Int
    addModIm x y =
      let
        z = x - im + y
      in
        if z < 0 then
          z + im
        else
          z

    tailRecurse : Int -> Int -> Int -> Int
    tailRecurse product x y =
      let
        smallX = x |> modBy im
        smallY = y |> modBy im
        a = min x y
        b = max x y
      in
        if a == 0 then
          product
        else
          tailRecurse
            ( if (a |> modBy 2) == 1 then
                addModIm product b
              else
                product
            )
            (a // 2)
            (addModIm b b)
  in
    tailRecurse 0

isAfter : Time.Posix -> Time.Posix -> Bool
isAfter t1 t2 =
  Time.posixToMillis t2 > Time.posixToMillis t1

sleepUntil : Time.Posix -> Time.Posix -> Task x ()
sleepUntil tf now =
  Time.posixToMillis tf - Time.posixToMillis now
  |> toFloat
  |> Process.sleep

waitForPing : Ping -> Task x Ping
waitForPing prevPing =
  let
    nextPing = next prevPing
  in
    Time.now
    |> Task.andThen (sleepUntil <| toTime nextPing)
    |> Task.map (always nextPing)



-- DEBUGGING STUFF THAT I SHOULD PACKAGE INTO TESTS

firstFewPings : List Ping
firstFewPings =
  let
    p0 = urPing
    p1 = next p0
    p2 = next p1
    p3 = next p2
    p4 = next p3
  in
    [p0, p1, p2, p3, p4]

{-- TODO: tests

List.Extra.unfoldr
  (\p -> if (next p |> Time.posixToMillis) > 1184105815000 then Nothing else Just (next p))
  urPing
== List.map (\sec -> Time.millisToPosix (sec*1000))
    [ 1184098754
    , 1184102685
    , 1184104776
    , 1184105302
    , 1184105815
    ]

(next <| advanceToLastBefore <| Time.millisToPosix 1184104776000) == Time.millisToPosix 1184105302000

--}
