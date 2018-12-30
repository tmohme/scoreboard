module PlayerTest exposing (leftPlayer, leftPlayerGen, maybePlayer, player, playerGen, playerId, playerIdGen, playerIdShrinker, playerShrinker, rightPlayer, rightPlayerGen, suite, validPlayer)

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
    describe "Player"
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
        , describe "updatedPlayer"
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
                            aPlayer.points + aPlayer.currentStreak + shotBalls
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
