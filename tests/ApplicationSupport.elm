module ApplicationSupport exposing (playerId, playerIdGen)

import Application as App
import Fuzz exposing (Fuzzer, bool, int, intRange, list, string, tuple, tuple3)
import Random exposing (Generator, map)
import Random.Extra exposing (andMap)
import Shrink exposing (Shrinker)


playerIdGen : Random.Generator App.PlayerId
playerIdGen =
    Random.Extra.choice App.Left App.Right


playerIdShrinker : Shrinker App.PlayerId
playerIdShrinker pid =
    Shrink.noShrink pid


playerId : Fuzzer App.PlayerId
playerId =
    Fuzz.custom playerIdGen playerIdShrinker
