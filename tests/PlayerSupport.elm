module PlayerSupport exposing (playerGen, playerShrinker, playerSwitchGen, playerSwitchShrinker)

import Application as App
import ApplicationSupport as AppSupport
import Player as P
import Random exposing (Generator, map)
import Random.Extra exposing (andMap)
import Shrink exposing (Shrinker)


bool : Generator Bool
bool =
    Random.int 0 1 |> map ((==) 0)


playerSwitchGen : Generator P.PlayerSwitch
playerSwitchGen =
    Random.map2
        (\switch foul ->
            if switch then
                P.Yes foul

            else
                P.No
        )
        bool
        bool


playerSwitchShrinker : Shrinker P.PlayerSwitch
playerSwitchShrinker switch =
    -- TODO implement a real shrinker
    Shrink.noShrink switch


playerGen : Generator P.Player
playerGen =
    Random.map validPlayer
        AppSupport.playerIdGen
        |> andMap (Random.int 0 31)
        |> andMap (Random.int 0 31)
        |> andMap (Random.int 0 7)
        |> andMap (Random.int 0 15)
        |> andMap (Random.constant 0)
        |> andMap (Random.int 0 2)


playerShrinker : Shrinker P.Player
playerShrinker aPlayer =
    Shrink.noShrink aPlayer


validPlayer : App.PlayerId -> Int -> Int -> Int -> Int -> Int -> Int -> P.Player
validPlayer pid points innings currentStreak longestStreak pointsAtStreakStart prevFouls =
    let
        vCurrentStreak =
            Basics.min points currentStreak

        vLongestStreak =
            Basics.max currentStreak longestStreak

        vPointsAtStreakStart =
            points - vCurrentStreak
    in
    P.Player pid points innings vCurrentStreak vLongestStreak vPointsAtStreakStart prevFouls