module MainTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, bool, int, list, string, intRange)
import Test exposing (..)
import Main exposing (..)
import Random.Pcg as Random
import Shrink


playerIdGen =
    Random.choice Left Right


playerIdShrinker playerId =
    Shrink.noShrink playerId


playerId : Fuzzer PlayerId
playerId =
    Fuzz.custom playerIdGen playerIdShrinker


leftPlayerGen =
    Random.map2 (Player Left) (Random.int 0 31) (Random.int 0 31)


rightPlayerGen =
    Random.map2 (Player Right) (Random.int 0 31) (Random.int 0 31)


playerGen =
    Random.map3 Player playerIdGen (Random.int 0 31) (Random.int 0 31)


playerShrinker player =
    Shrink.noShrink player


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
            Random.maybe Random.bool playerGen

        shrinker maybePlayer =
            Shrink.maybe playerShrinker maybePlayer
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
            [ fuzz3 (intRange 0 2) leftPlayer rightPlayer "is always 'shootingPreviously' when no switch" <|
                \n left right ->
                    let
                        shootingPreviously =
                            case n of
                                1 ->
                                    Just left

                                2 ->
                                    Just right

                                _ ->
                                    Nothing
                    in
                        (determineShootingNext shootingPreviously False left right)
                            |> Maybe.map .id
                            |> Expect.equal (shootingPreviously |> Maybe.map .id)
            ]
        , describe "updatedPlayer"
            [ fuzz3 player int int "is always the same player when no-one was shooting" <|
                \player shotBalls inningIncrement ->
                    updatedPlayer player Nothing shotBalls inningIncrement
                        |> Expect.equal player
            , fuzz4 player player int int "is always the same player when someone was shooting" <|
                \player shooting shotBalls inningIncrement ->
                    updatedPlayer player (Just shooting) shotBalls inningIncrement
                        |> Expect.equal player
            , fuzz3 player int int "has shooting player's points incremented" <|
                \player shotBalls inningIncrement ->
                    (updatedPlayer player (Just player) shotBalls inningIncrement).points
                        |> Expect.equal (player.points + shotBalls)
            , fuzz3 player int int "has shooting player's innings incremented" <|
                \player shotBalls inningIncrement ->
                    (updatedPlayer player (Just player) shotBalls inningIncrement).innings
                        |> Expect.equal (player.innings + inningIncrement)
            ]
        ]
