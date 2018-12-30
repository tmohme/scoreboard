module GameTest exposing (leftPlayer, leftPlayerGen, maybePlayer, player, playerGen, playerId, playerIdGen, playerIdShrinker, playerShrinker, rightPlayer, rightPlayerGen, suite, validPlayer)

import Application as App
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, bool, int, intRange, list, string, tuple, tuple3)
import Game exposing (..)
import Main
import Player exposing (Player)
import Random exposing (Generator, map)
import Random.Extra exposing (andMap)
import Shrink exposing (Shrinker)
import Test exposing (..)


playerIdGen : Random.Generator App.PlayerId
playerIdGen =
    Random.Extra.choice App.Left App.Right


playerIdShrinker : Shrinker App.PlayerId
playerIdShrinker pid =
    Shrink.noShrink pid


playerId : Fuzzer App.PlayerId
playerId =
    Fuzz.custom playerIdGen playerIdShrinker


leftPlayerGen : Generator Player
leftPlayerGen =
    Random.map5 (Player App.Left) (Random.int 0 31) (Random.int 0 31) (Random.int 0 7) (Random.int 0 15) (Random.constant 0)


rightPlayerGen : Generator Player
rightPlayerGen =
    Random.map5 (Player App.Right) (Random.int 0 31) (Random.int 0 31) (Random.int 0 7) (Random.int 0 15) (Random.constant 0)


validPlayer : App.PlayerId -> Int -> Int -> Int -> Int -> Int -> Player
validPlayer pid points innings currentStreak longestStreak pointsAtStreakStart =
    let
        vCurrentStreak =
            Basics.min points currentStreak

        vLongestStreak =
            Basics.max currentStreak longestStreak

        vPointsAtStreakStart =
            points - vCurrentStreak
    in
    Player pid points innings vCurrentStreak vLongestStreak vPointsAtStreakStart


playerGen : Generator Player
playerGen =
    Random.map validPlayer
        playerIdGen
        |> andMap (Random.int 0 31)
        |> andMap (Random.int 0 31)
        |> andMap (Random.int 0 7)
        |> andMap (Random.int 0 15)
        |> andMap (Random.constant 0)



--    Random.map6 Player playerIdGen (Random.int 0 31) (Random.int 0 31) (Random.int 0 7) (Random.int 0 15) 0


playerShrinker : Shrinker Player
playerShrinker aPlayer =
    Shrink.noShrink aPlayer


leftPlayer : Fuzzer Player
leftPlayer =
    Fuzz.custom leftPlayerGen playerShrinker


rightPlayer : Fuzzer Player
rightPlayer =
    Fuzz.custom rightPlayerGen playerShrinker


player : Fuzzer Player
player =
    Fuzz.custom playerGen playerShrinker


maybePlayer : Fuzzer (Maybe Player)
maybePlayer =
    let
        generator =
            Random.Extra.maybe Random.Extra.bool playerGen

        shrinker aMaybePlayer =
            Shrink.maybe playerShrinker aMaybePlayer
    in
    Fuzz.custom generator shrinker


suite : Test
suite =
    describe "Scoreboard"
        [ describe "No winner"
            [ fuzz2
                (intRange 0 10)
                (intRange 0 10)
                "as long as no player reached 'toPoints'"
              <|
                \lPoints rPoints ->
                    let
                        left =
                            Player App.Left lPoints 0 0 0 0

                        right =
                            Player App.Right rPoints 0 0 0 0

                        toPoints =
                            max (lPoints + 1) (rPoints + 1)
                    in
                    determineWinner toPoints left right
                        |> Expect.equal Nothing
            ]

        --
        , describe "determine shooting next"
            [ fuzz3
                playerId
                leftPlayer
                rightPlayer
                "is always 'shootingPreviously' when no switch"
              <|
                \shootingPreviously left right ->
                    determineShootingNext shootingPreviously False left right
                        |> .id
                        |> Expect.equal shootingPreviously

            --
            , fuzz3
                playerId
                leftPlayer
                rightPlayer
                "is never 'shootingPreviously' when switching from a player"
              <|
                \shootingPreviously left right ->
                    determineShootingNext shootingPreviously True left right
                        |> .id
                        |> Expect.notEqual shootingPreviously
            ]
        ]
