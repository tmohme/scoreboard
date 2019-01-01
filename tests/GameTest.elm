module GameTest exposing (suite)

import Application as App
import ApplicationSupport as AS
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, bool, int, intRange, list, string, tuple, tuple3)
import Game
import Player
import PlayerSupport as PS
import Random exposing (Generator, map)
import Random.Extra exposing (andMap)
import Shrink exposing (Shrinker)
import Test exposing (..)


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
                    Game.determineWinner toPoints left right
                        |> Expect.equal Nothing
            ]

        --
        , describe "determine shooting next"
            [ fuzz3
                AS.playerId
                PS.leftPlayer
                PS.rightPlayer
                "is always 'shootingPreviously' when no switch"
              <|
                \shootingPreviously left right ->
                    let
                        playerSwitch =
                            Player.No
                    in
                    Game.determineShootingNext shootingPreviously playerSwitch left right
                        |> .id
                        |> Expect.equal shootingPreviously

            --
            , fuzz2
                (tuple3 ( AS.playerId, PS.leftPlayer, PS.rightPlayer ))
                PS.switchReason
                "is never 'shootingPreviously' when switching from a player"
              <|
                \( shootingPreviously, left, right ) reason ->
                    let
                        playerSwitch =
                            Player.Yes reason
                    in
                    Game.determineShootingNext shootingPreviously playerSwitch left right
                        |> .id
                        |> Expect.notEqual shootingPreviously
            ]
        ]
