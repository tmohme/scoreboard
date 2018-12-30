module PlayerSupport exposing (playerGen, playerShrinker)

import Application as App
import ApplicationSupport as AppSupport
import Player as P
import Random exposing (Generator, map)
import Random.Extra exposing (andMap)
import Shrink exposing (Shrinker)


playerGen : Generator P.Player
playerGen =
    Random.map validPlayer
        AppSupport.playerIdGen
        |> andMap (Random.int 0 31)
        |> andMap (Random.int 0 31)
        |> andMap (Random.int 0 7)
        |> andMap (Random.int 0 15)
        |> andMap (Random.constant 0)


playerShrinker : Shrinker P.Player
playerShrinker aPlayer =
    Shrink.noShrink aPlayer


validPlayer : App.PlayerId -> Int -> Int -> Int -> Int -> Int -> P.Player
validPlayer pid points innings currentStreak longestStreak pointsAtStreakStart =
    let
        vCurrentStreak =
            Basics.min points currentStreak

        vLongestStreak =
            Basics.max currentStreak longestStreak

        vPointsAtStreakStart =
            points - vCurrentStreak
    in
    P.Player pid points innings vCurrentStreak vLongestStreak vPointsAtStreakStart
