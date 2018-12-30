module GameTest exposing (leftPlayer, maybePlayer, player, rightPlayer, suite)

import Application as App
import ApplicationSupport as AS
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, bool, int, intRange, list, string, tuple, tuple3)
import Game exposing (..)
import Main
import Player exposing (Player)
import PlayerSupport as PS
import Random exposing (Generator, map)
import Random.Extra exposing (andMap)
import Shrink exposing (Shrinker)
import Test exposing (..)


leftPlayer : Fuzzer Player
leftPlayer =
    Fuzz.custom AS.leftPlayerGen PS.playerShrinker


rightPlayer : Fuzzer Player
rightPlayer =
    Fuzz.custom AS.rightPlayerGen PS.playerShrinker


player : Fuzzer Player
player =
    Fuzz.custom PS.playerGen PS.playerShrinker


maybePlayer : Fuzzer (Maybe Player)
maybePlayer =
    let
        generator =
            Random.Extra.maybe Random.Extra.bool PS.playerGen

        shrinker aMaybePlayer =
            Shrink.maybe PS.playerShrinker aMaybePlayer
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
                AS.playerId
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
                AS.playerId
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
