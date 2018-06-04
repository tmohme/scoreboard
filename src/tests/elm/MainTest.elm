module MainTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, bool, int, list, string, intRange)
import Test exposing (..)
import Main exposing (..)
import Random.Pcg as Random exposing (Generator)
import Shrink exposing (Shrinker)


playerIdGen : Generator PlayerId
playerIdGen =
    Random.choice Left Right


playerIdShrinker : Shrinker PlayerId
playerIdShrinker playerId =
    Shrink.noShrink playerId


playerId : Fuzzer PlayerId
playerId =
    Fuzz.custom playerIdGen playerIdShrinker


leftPlayerGen : Generator Player
leftPlayerGen =
    Random.map5 (Player Left) (Random.int 0 31) (Random.int 0 31) (Random.int 0 7) (Random.int 0 15) (Random.constant 0)


rightPlayerGen : Generator Player
rightPlayerGen =
    Random.map5 (Player Right) (Random.int 0 31) (Random.int 0 31) (Random.int 0 7) (Random.int 0 15) (Random.constant 0)


validPlayer : PlayerId -> Int -> Int -> Int -> Int -> Int -> Player
validPlayer id points innings currentStreak longestStreak pointsAtStreakStart =
    let
        vCurrentStreak =
            Basics.min points currentStreak

        vLongestStreak =
            Basics.max currentStreak longestStreak

        vPointsAtStreakStart =
            points - vCurrentStreak
    in
        Player id points innings vCurrentStreak vLongestStreak vPointsAtStreakStart


playerGen : Generator Player
playerGen =
    Random.map validPlayer playerIdGen
        |> Random.andMap (Random.int 0 31)
        |> Random.andMap (Random.int 0 31)
        |> Random.andMap (Random.int 0 7)
        |> Random.andMap (Random.int 0 15)
        |> Random.andMap (Random.constant 0)



--    Random.map6 Player playerIdGen (Random.int 0 31) (Random.int 0 31) (Random.int 0 7) (Random.int 0 15) 0


playerShrinker : Shrinker Player
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
                            Player Left lPoints 0 0 0 0

                        right =
                            Player Right rPoints 0 0 0 0
                    in
                        determineWinner Nothing left right
                            |> Expect.equal Nothing

            --
            , fuzz2
                (intRange 0 10)
                (intRange 0 10)
                "as long as no player reached 'toPoints'"
              <|
                \lPoints rPoints ->
                    let
                        left =
                            Player Left lPoints 0 0 0 0

                        right =
                            Player Right rPoints 0 0 0 0

                        toPoints =
                            max (lPoints + 1) (rPoints + 1)
                    in
                        determineWinner (Just toPoints) left right
                            |> Expect.equal Nothing
            ]

        --
        , describe "determine shooting next"
            [ fuzz3
                (intRange 0 2)
                leftPlayer
                rightPlayer
                "is always 'shootingPreviously' when no switch"
              <|
                \n left right ->
                    let
                        shootingPreviously =
                            case n of
                                1 ->
                                    Just left.id

                                2 ->
                                    Just right.id

                                _ ->
                                    Nothing
                    in
                        (determineShootingNext shootingPreviously False left right)
                            |> Maybe.map .id
                            |> Expect.equal shootingPreviously

            --
            , fuzz3
                bool
                leftPlayer
                rightPlayer
                "is never 'shootingPreviously' when switching from a player"
              <|
                \bool left right ->
                    let
                        shootingPreviously =
                            case bool of
                                True ->
                                    Just left.id

                                False ->
                                    Just right.id
                    in
                        (determineShootingNext shootingPreviously True left right)
                            |> Maybe.map .id
                            |> Expect.notEqual shootingPreviously
            ]

        --
        , describe "calculateCurrentStreak"
            [ fuzz2
                (intRange 0 10)
                (intRange 0 10)
                "is at most limit"
              <|
                \previous increment ->
                    let
                        limit =
                            (Basics.max previous increment)
                    in
                        (calculateCurrentStreak previous increment limit) |> Expect.atMost limit

            --
            , fuzz2
                (intRange 0 10)
                (intRange 0 10)
                "is at least previous"
              <|
                \previous increment ->
                    let
                        limit =
                            (Basics.max previous increment) + 1
                    in
                        (calculateCurrentStreak previous increment limit) |> Expect.atLeast previous

            --
            , fuzz2
                (intRange 0 10)
                (intRange 0 10)
                "is at least min(previous, increment)"
              <|
                \previous increment ->
                    let
                        limit =
                            (Basics.max previous increment)
                    in
                        (calculateCurrentStreak previous increment limit)
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
                            (Basics.max previous increment)
                    in
                        (calculateCurrentStreak previous increment limit)
                            |> Expect.atLeast (Basics.min increment limit)
            ]

        --
        , describe "updatedPlayer"
            [ fuzz4
                player
                int
                bool
                (intRange 1 150)
                "is always the same player when no-one was shooting"
              <|
                \player shotBalls switchPlayer runTo ->
                    (updatedPlayer player Nothing shotBalls switchPlayer runTo).id
                        |> Expect.equal (player.id)

            --
            , fuzz5
                player
                player
                int
                bool
                (intRange 1 150)
                "is always the same player when someone was shooting"
              <|
                \player shooting shotBalls switchPlayer runTo ->
                    (updatedPlayer player (Just shooting) shotBalls switchPlayer runTo).id
                        |> Expect.equal (player.id)

            --
            , fuzz3
                player
                int
                bool
                "has shooting player's points incremented when not yet won"
              <|
                \player shotBalls switchPlayer ->
                    let
                        runTo =
                            player.points + shotBalls + 1
                    in
                        (updatedPlayer player (Just player) shotBalls switchPlayer runTo).points
                            |> Expect.equal (player.points + shotBalls)

            --
            , fuzz3
                player
                int
                bool
                "has shooting player's points incremented when exactly won"
              <|
                \player shotBalls switchPlayer ->
                    let
                        runTo =
                            player.points + shotBalls
                    in
                        (updatedPlayer player (Just player) shotBalls switchPlayer runTo).points
                            |> Expect.equal (player.points + shotBalls)

            --
            , fuzz3
                player
                (intRange 2 10)
                bool
                "has shooting player's points limited incremented when overshot"
              <|
                \player shotBalls switchPlayer ->
                    let
                        runTo =
                            player.points + shotBalls - 1
                    in
                        (updatedPlayer player (Just player) shotBalls switchPlayer runTo).points
                            |> Expect.equal runTo

            --
            , fuzz4
                player
                int
                bool
                (intRange 1 150)
                "has shooting player's innings incremented"
              <|
                \player shotBalls switchPlayer runTo ->
                    (updatedPlayer player (Just player) shotBalls True runTo).innings
                        |> Expect.equal (player.innings + 1)

            --
            , fuzz4
                player
                int
                bool
                (intRange 1 150)
                "strictly monotonically increments longestStreak"
              <|
                \player shotBalls switchPlayer runTo ->
                    let
                        prevLongestStreak =
                            player.longestStreak
                    in
                        (updatedPlayer player (Just player) shotBalls switchPlayer runTo).longestStreak
                            |> Expect.atLeast prevLongestStreak

            --
            , fuzz2
                player
                int
                "sets currentStreak when no switch and not won"
              <|
                \player shotBalls ->
                    let
                        prevCurrentStreak =
                            player.currentStreak

                        runTo =
                            player.points + player.currentStreak + shotBalls + 1
                    in
                        (updatedPlayer player (Just player) shotBalls False runTo).currentStreak
                            |> Expect.equal (prevCurrentStreak + shotBalls)

            --
            , fuzz2
                player
                int
                "sets currentStreak when no switch and exactly won"
              <|
                \player shotBalls ->
                    let
                        prevCurrentStreak =
                            player.currentStreak

                        runTo =
                            player.points + player.currentStreak + shotBalls
                    in
                        (updatedPlayer player (Just player) shotBalls False runTo).currentStreak
                            |> Expect.equal (prevCurrentStreak + shotBalls)

            --
            , fuzz4
                leftPlayer
                rightPlayer
                int
                (intRange 1 150)
                "resets currentStreak after switchPlayer"
              <|
                \leftPlayer rightPlayer shotBalls runTo ->
                    (updatedPlayer leftPlayer (Just rightPlayer) shotBalls True runTo).currentStreak
                        |> Expect.equal 0
            ]
        ]
