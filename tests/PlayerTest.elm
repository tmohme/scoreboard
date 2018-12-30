module PlayerTest exposing (leftPlayer, maybePlayer, player, rightPlayer, suite)

import ApplicationSupport as AS
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, bool, int, intRange, list, string, tuple, tuple3)
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
    describe "A Player"
        [ describe "calculateCurrentStreak"
            [ fuzz2
                (intRange 0 10)
                (intRange 0 10)
                "is at most limit"
              <|
                \previous increment ->
                    let
                        limit =
                            Basics.max previous increment
                    in
                    Player.calculateCurrentStreak previous increment limit |> Expect.atMost limit

            --
            , fuzz2
                (intRange 0 10)
                (intRange 0 10)
                "is at least previous"
              <|
                \previous increment ->
                    let
                        limit =
                            Basics.max previous increment + 1
                    in
                    Player.calculateCurrentStreak previous increment limit |> Expect.atLeast previous

            --
            , fuzz2
                (intRange 0 10)
                (intRange 0 10)
                "is at least min(previous, increment)"
              <|
                \previous increment ->
                    let
                        limit =
                            Basics.max previous increment
                    in
                    Player.calculateCurrentStreak previous increment limit
                        |> Expect.atLeast (Basics.min previous increment)

            --
            , fuzz2
                (intRange 0 10)
                (intRange 0 10)
                "is at least min(increment, limit)"
              <|
                \previous increment ->
                    let
                        limit =
                            Basics.max previous increment
                    in
                    Player.calculateCurrentStreak previous increment limit
                        |> Expect.atLeast (Basics.min increment limit)
            ]

        --
        , describe "when updated"
            [ fuzz3
                player
                (tuple3 ( player, int, bool ))
                (intRange 1 150)
                "is always the same player when someone was shooting"
              <|
                \aPlayer ( shooting, shotBalls, switchPlayer ) runTo ->
                    (Player.update aPlayer shooting shotBalls switchPlayer runTo).id
                        |> Expect.equal aPlayer.id

            --
            , fuzz3
                player
                int
                bool
                "has shooting player's points incremented when not yet won"
              <|
                \aPlayer shotBalls switchPlayer ->
                    let
                        runTo =
                            aPlayer.points + shotBalls + 1
                    in
                    (Player.update aPlayer aPlayer shotBalls switchPlayer runTo).points
                        |> Expect.equal (aPlayer.points + shotBalls)

            --
            , fuzz3
                player
                int
                bool
                "has shooting player's points incremented when exactly won"
              <|
                \aPlayer shotBalls switchPlayer ->
                    let
                        runTo =
                            aPlayer.points + shotBalls
                    in
                    (Player.update aPlayer aPlayer shotBalls switchPlayer runTo).points
                        |> Expect.equal (aPlayer.points + shotBalls)

            --
            , fuzz3
                player
                (intRange 2 10)
                bool
                "has shooting player's points limited incremented when overshot"
              <|
                \aPlayer shotBalls switchPlayer ->
                    let
                        runTo =
                            aPlayer.points + shotBalls - 1
                    in
                    (Player.update aPlayer aPlayer shotBalls switchPlayer runTo).points
                        |> Expect.equal runTo

            --
            , fuzz3
                player
                int
                (intRange 1 150)
                "has shooting player's innings incremented after a switch"
              <|
                \aPlayer shotBalls runTo ->
                    (Player.update aPlayer aPlayer shotBalls True runTo).innings
                        |> Expect.equal (aPlayer.innings + 1)

            --
            , fuzz2
                (tuple3 ( player, int, bool ))
                (intRange 1 150)
                "strictly monotonically increments longestStreak"
              <|
                \( aPlayer, shotBalls, switchPlayer ) runTo ->
                    let
                        prevLongestStreak =
                            aPlayer.longestStreak
                    in
                    (Player.update aPlayer aPlayer shotBalls switchPlayer runTo).longestStreak
                        |> Expect.atLeast prevLongestStreak

            --
            , fuzz2
                player
                int
                "sets currentStreak when no switch and not won"
              <|
                \aPlayer shotBalls ->
                    let
                        prevCurrentStreak =
                            aPlayer.currentStreak

                        runTo =
                            aPlayer.points + aPlayer.currentStreak + shotBalls + 1
                    in
                    (Player.update aPlayer aPlayer shotBalls False runTo).currentStreak
                        |> Expect.equal (prevCurrentStreak + shotBalls)

            --
            , fuzz2
                player
                int
                "sets currentStreak when no switch and exactly won"
              <|
                \aPlayer shotBalls ->
                    let
                        prevCurrentStreak =
                            aPlayer.currentStreak

                        runTo =
                            aPlayer.points
                                + aPlayer.currentStreak
                                + shotBalls
                    in
                    (Player.update aPlayer aPlayer shotBalls False runTo).currentStreak
                        |> Expect.equal (prevCurrentStreak + shotBalls)

            --
            , fuzz3
                (tuple ( leftPlayer, rightPlayer ))
                int
                (intRange 1 150)
                "resets currentStreak after switchPlayer"
              <|
                \( left, right ) shotBalls runTo ->
                    (Player.update left right shotBalls True runTo).currentStreak
                        |> Expect.equal 0
            ]
        ]
