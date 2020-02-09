module GameTest exposing (suite)

import Application as App
import ApplicationSupport as AS
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, intRange, tuple3)
import Game
import Player
import PlayerSupport as PS
import Test exposing (..)


suite : Test
suite =
    describe "Multiplayer rules"
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

        --
        , describe "a foul"
            [ fuzz3
                PS.leftPlayer
                PS.rightPlayer
                (intRange 1 Game.fullRack)
                "always switches to the other player - regardless of how many balls left on table"
              <|
                \leftPlayer rightPlayer ballsOnTable ->
                    let
                        game =
                            -- TODO replace constant
                            Game.init { breakingPlayerId = App.Left, runTo = 150 }

                        gameState =
                            game.state

                        configuredState =
                            { gameState | left = leftPlayer, right = rightPlayer, switchReason = Player.Foul }

                        configuredGame =
                            { game | state = configuredState }
                    in
                    configuredGame
                        |> Game.update (Game.BallsLeftOnTable ballsOnTable)
                        |> Tuple.first
                        |> .state
                        |> .shooting
                        |> .id
                        |> Expect.equal rightPlayer.id

            --
            , fuzz3
                PS.leftPlayer
                PS.rightPlayer
                (intRange 1 Game.fullRack)
                "(if third in a row for a player) leads to a full rack is set-up"
              <|
                \leftPlayer rightPlayer ballsOnTable ->
                    let
                        foulingPlayer =
                            { leftPlayer | previousFouls = 2 }

                        game =
                            -- TODO replace constant
                            Game.init { breakingPlayerId = App.Left, runTo = 150 }

                        gameState =
                            game.state

                        configuredState =
                            { gameState | left = foulingPlayer, right = rightPlayer, switchReason = Player.Foul }

                        configuredGame =
                            { game | state = configuredState }
                    in
                    configuredGame
                        |> Game.update (Game.BallsLeftOnTable ballsOnTable)
                        |> Tuple.first
                        |> .state
                        |> .ballsOnTable
                        |> Expect.equal Game.fullRack
            ]
        ]
