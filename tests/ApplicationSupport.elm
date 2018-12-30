module ApplicationSupport exposing (leftPlayerGen, playerId, playerIdGen, rightPlayerGen)

import Application as App
import Fuzz exposing (Fuzzer, bool, int, intRange, list, string, tuple, tuple3)
import Player as P
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


leftPlayerGen : Generator P.Player
leftPlayerGen =
    Random.map5 (P.Player App.Left) (Random.int 0 31) (Random.int 0 31) (Random.int 0 7) (Random.int 0 15) (Random.constant 0)


rightPlayerGen : Generator P.Player
rightPlayerGen =
    Random.map5 (P.Player App.Right) (Random.int 0 31) (Random.int 0 31) (Random.int 0 7) (Random.int 0 15) (Random.constant 0)
