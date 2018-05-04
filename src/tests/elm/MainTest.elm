module MainTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, bool, int, list, string, intRange)
import Test exposing (..)
import Main exposing (..)
import Random.Pcg as Random
import Shrink


playerIdGen =
    Random.bool
        |> Random.andThen
            (\b ->
                if b then
                    Random.constant Left
                else
                    Random.constant Right
            )


playerId : Fuzzer PlayerId
playerId =
    let
        generator =
            playerIdGen

        shrinker playerId =
            Shrink.noShrink playerId
    in
        Fuzz.custom generator shrinker


player : Fuzzer Player
player =
    let
        generator =
            Random.map3 Player playerIdGen (Random.int 0 31) (Random.int 0 31)

        shrinker playerId =
            Shrink.noShrink playerId
    in
        Fuzz.custom generator shrinker


suite : Test
suite =
    describe "Scoreboard"
        [ describe "No winner"
            [ fuzz2
                (intRange 0 10)
                (intRange 0 10)
                "until 'toPoints' are set"
              <|
                \lPoints rPoints ->
                    let
                        left =
                            Player Left lPoints 0

                        right =
                            Player Right rPoints 0
                    in
                        determineWinner Nothing left right
                            |> Expect.equal Nothing
            , fuzz2
                (intRange 0 10)
                (intRange 0 10)
                "as long as no player reached 'toPoints'"
              <|
                \lPoints rPoints ->
                    let
                        left =
                            Player Left lPoints 0

                        right =
                            Player Right rPoints 0

                        toPoints =
                            max (lPoints + 1) (rPoints + 1)
                    in
                        determineWinner (Just toPoints) left right
                            |> Expect.equal Nothing
            ]
        , describe "determine shooting next"
            [ fuzz3 bool player player "is always 'shootingPreviously' when no switch" <|
                \bool left right ->
                    let
                        shootingPreviously =
                            if (bool) then
                                Just left
                            else
                                Just right
                    in
                        determineShootingNext shootingPreviously False left right
                            |> Expect.equal shootingPreviously
            ]
        ]
