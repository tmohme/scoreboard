module PlayerSupport exposing (leftPlayer, player, playerSwitch, rightPlayer, switchReason)

import Application as App
import ApplicationSupport as AppSupport
import Fuzz exposing (Fuzzer)
import Player as P


switchReason : Fuzzer P.SwitchReason
switchReason =
    Fuzz.oneOf
        [ Fuzz.constant P.Miss
        , Fuzz.constant P.Foul
        ]


playerSwitch : Fuzzer P.PlayerSwitch
playerSwitch =
    Fuzz.oneOf
        [ Fuzz.constant P.No
        , Fuzz.map P.Yes switchReason
        ]


validPlayer : App.PlayerId -> Int -> Int -> Int -> Int -> Int -> P.Player
validPlayer pid pointsSoFar inningsSoFar currentStreakSoFar longestStreakSoFar foulsSoFar =
    let
        vCurrentStreak =
            Basics.min pointsSoFar currentStreakSoFar

        vLongestStreak =
            Basics.max currentStreakSoFar longestStreakSoFar

        vPointsAtStreakStart =
            pointsSoFar - vCurrentStreak
    in
    P.Player pid (App.nameOf pid) pointsSoFar inningsSoFar vCurrentStreak vLongestStreak vPointsAtStreakStart foulsSoFar


points : Fuzzer Int
points =
    Fuzz.intRange 0 31


innings : Fuzzer Int
innings =
    Fuzz.intRange 0 31


currentStreak : Fuzzer Int
currentStreak =
    Fuzz.intRange 0 7


longestStreak : Fuzzer Int
longestStreak =
    Fuzz.intRange 0 15


prevFouls : Fuzzer Int
prevFouls =
    Fuzz.intRange 0 2


player : Fuzzer P.Player
player =
    Fuzz.map validPlayer
        AppSupport.playerId
        |> Fuzz.andMap points
        |> Fuzz.andMap innings
        |> Fuzz.andMap currentStreak
        |> Fuzz.andMap longestStreak
        |> Fuzz.andMap prevFouls


leftPlayer : Fuzzer P.Player
leftPlayer =
    Fuzz.map validPlayer
        (Fuzz.constant App.Left)
        |> Fuzz.andMap points
        |> Fuzz.andMap innings
        |> Fuzz.andMap currentStreak
        |> Fuzz.andMap longestStreak
        |> Fuzz.andMap prevFouls


rightPlayer : Fuzzer P.Player
rightPlayer =
    Fuzz.map validPlayer
        (Fuzz.constant App.Right)
        |> Fuzz.andMap points
        |> Fuzz.andMap innings
        |> Fuzz.andMap currentStreak
        |> Fuzz.andMap longestStreak
        |> Fuzz.andMap prevFouls
