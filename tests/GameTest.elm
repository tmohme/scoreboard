module GameTest exposing (leftPlayer, leftPlayerGen, maybePlayer, player, rightPlayer, rightPlayerGen, suite)

import Application as App
import ApplicationSupport as AppSupport
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, bool, int, intRange, list, string, tuple, tuple3)
import Game exposing (..)
import Main
import Player exposing (Player)
import PlayerSupport
import Random exposing (Generator, map)
import Random.Extra exposing (andMap)
import Shrink exposing (Shrinker)
import Test exposing (..)


leftPlayerGen : Generator Player
leftPlayerGen =
    Random.map5 (Player App.Left) (Random.int 0 31) (Random.int 0 31) (Random.int 0 7) (Random.int 0 15) (Random.constant 0)


rightPlayerGen : Generator Player
rightPlayerGen =
    Random.map5 (Player App.Right) (Random.int 0 31) (Random.int 0 31) (Random.int 0 7) (Random.int 0 15) (Random.constant 0)


leftPlayer : Fuzzer Player
leftPlayer =
    Fuzz.custom leftPlayerGen PlayerSupport.playerShrinker


rightPlayer : Fuzzer Player
rightPlayer =
    Fuzz.custom rightPlayerGen PlayerSupport.playerShrinker


player : Fuzzer Player
player =
    Fuzz.custom PlayerSupport.playerGen PlayerSupport.playerShrinker


maybePlayer : Fuzzer (Maybe Player)
maybePlayer =
    let
        generator =
            Random.Extra.maybe Random.Extra.bool PlayerSupport.playerGen

        shrinker aMaybePlayer =
            Shrink.maybe PlayerSupport.playerShrinker aMaybePlayer
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
                        bareLeft =
                            Player.create App.Left

                        left =
                            { bareLeft | points = lPoints }

                        bareRight =
                            Player.create App.Right

                        right =
                            { bareRight | points = rPoints }

                        toPoints =
                            max (lPoints + 1) (rPoints + 1)
                    in
                    determineWinner toPoints left right
                        |> Expect.equal Nothing
            ]

        --
        , describe "determine shooting next"
            [ fuzz3
                AppSupport.playerId
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
                AppSupport.playerId
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
