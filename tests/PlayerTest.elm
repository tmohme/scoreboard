module PlayerTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, bool, int, intRange, tuple, tuple3)
import Player exposing (Player, PlayerSwitch(..))
import PlayerSupport exposing (leftPlayer, player, playerSwitch, rightPlayer, switchReason)
import Test exposing (..)


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
                (tuple3 ( player, int, playerSwitch ))
                (intRange 1 150)
                "is always the same player indepedent of who was shooting"
              <|
                \aPlayer ( shooting, shotBalls, switch ) runTo ->
                    let
                        ( updatedPlayer, _ ) =
                            Player.update aPlayer shooting shotBalls switch runTo
                    in
                    updatedPlayer.id
                        |> Expect.equal aPlayer.id

            --
            , fuzz3
                player
                int
                bool
                "has his points incremented by shotBalls when not yet won (and no foul)"
              <|
                \aPlayer shotBalls switch ->
                    let
                        runTo =
                            aPlayer.points + shotBalls + 1

                        switchPlayer =
                            case switch of
                                True ->
                                    Player.Yes Player.Miss

                                False ->
                                    Player.No

                        ( updatedPlayer, _ ) =
                            Player.update aPlayer aPlayer shotBalls switchPlayer runTo
                    in
                    updatedPlayer.points
                        |> Expect.equal (aPlayer.points + shotBalls)

            --
            , fuzz3
                player
                (intRange 1 150)
                bool
                "has his points incremented by shotBalls when exactly won (and no foul)"
              <|
                \aPlayer shotBalls switch ->
                    let
                        runTo =
                            aPlayer.points + shotBalls

                        switchPlayer =
                            case switch of
                                True ->
                                    Player.Yes Player.Miss

                                False ->
                                    Player.No

                        ( updatedPlayer, _ ) =
                            Player.update aPlayer aPlayer shotBalls switchPlayer runTo
                    in
                    updatedPlayer.points
                        |> Expect.equal (aPlayer.points + shotBalls)

            --
            , fuzz3
                player
                (intRange 2 10)
                bool
                "has his points capped incremented when overshot (and no foul)"
              <|
                \aPlayer shotBalls switch ->
                    let
                        runTo =
                            aPlayer.points + shotBalls - 1

                        switchPlayer =
                            case switch of
                                True ->
                                    Player.Yes Player.Miss

                                False ->
                                    Player.No

                        ( updatedPlayer, _ ) =
                            Player.update aPlayer aPlayer shotBalls switchPlayer runTo
                    in
                    updatedPlayer.points
                        |> Expect.equal runTo

            --
            , fuzz2
                (tuple3 ( player, int, switchReason ))
                (intRange 1 150)
                "has his innings incremented after a switch"
              <|
                \( aPlayer, shotBalls, withFoul ) runTo ->
                    let
                        switch =
                            Yes withFoul

                        ( updatedPlayer, _ ) =
                            Player.update aPlayer aPlayer shotBalls switch runTo
                    in
                    updatedPlayer.innings
                        |> Expect.equal (aPlayer.innings + 1)

            --
            , fuzz2
                player
                int
                "has his previous fouls reset when switched without foul (and not overshot)"
              <|
                \aPlayer shotBalls ->
                    let
                        switch =
                            Yes Player.Miss

                        runTo =
                            aPlayer.points + shotBalls + 1

                        ( updatedPlayer, _ ) =
                            Player.update aPlayer aPlayer shotBalls switch runTo
                    in
                    updatedPlayer.previousFouls
                        |> Expect.equal 0

            --
            , fuzz2
                player
                int
                "has his previous fouls unchanged when overshot (already won)"
              <|
                \aPlayer shotBalls ->
                    let
                        switch =
                            Yes Player.Foul

                        runTo =
                            aPlayer.points + shotBalls - 1

                        playerWithTwoFouls =
                            { aPlayer | previousFouls = 2 }

                        ( updatedPLayer, _ ) =
                            Player.update playerWithTwoFouls playerWithTwoFouls shotBalls switch runTo
                    in
                    updatedPLayer.previousFouls
                        |> Expect.equal playerWithTwoFouls.previousFouls

            --
            , fuzz3
                player
                int
                (intRange 0 1)
                "has his previous fouls incremented when switched with foul (and already has 0 or 1 foul, not overshot)"
              <|
                \generatedPlayer shotBalls prevFouls ->
                    let
                        switch =
                            Yes Player.Foul

                        aPlayer =
                            { generatedPlayer | previousFouls = prevFouls }

                        runTo =
                            aPlayer.points + shotBalls + 1

                        ( updatedPlayer, _ ) =
                            Player.update aPlayer aPlayer shotBalls switch runTo
                    in
                    updatedPlayer.previousFouls
                        |> Expect.equal (aPlayer.previousFouls + 1)

            --
            , fuzz2
                (tuple ( player, intRange 0 1 ))
                int
                "has his points decremented by one for a foul (when not capped by game target)"
              <|
                \( generatedPlayer, prevFouls ) shotBalls ->
                    let
                        aPlayer =
                            { generatedPlayer | previousFouls = prevFouls }

                        switch =
                            Yes Player.Foul

                        runTo =
                            aPlayer.points + shotBalls

                        ( updatedPlayer, _ ) =
                            Player.update aPlayer aPlayer shotBalls switch runTo
                    in
                    updatedPlayer.points
                        |> Expect.equal (aPlayer.points + shotBalls - 1)

            --
            , fuzz2
                player
                int
                "has his points decremented by (1 + 15) after the third consecutive foul (when not capped by game target)"
              <|
                \generatedPlayer shotBalls ->
                    let
                        aPlayer =
                            { generatedPlayer | previousFouls = 2 }

                        switch =
                            Yes Player.Foul

                        runTo =
                            aPlayer.points + shotBalls

                        ( updatedPlayer, _ ) =
                            Player.update aPlayer aPlayer shotBalls switch runTo
                    in
                    updatedPlayer.points
                        |> Expect.equal (aPlayer.points + shotBalls - (1 + 15))

            --
            , fuzz2
                player
                int
                "has his fouls reset after the third consecutive foul"
              <|
                \aPlayer shotBalls ->
                    let
                        switch =
                            Yes Player.Foul

                        runTo =
                            aPlayer.points + shotBalls

                        playerWithTwoFouls =
                            { aPlayer | previousFouls = 2 }

                        ( updatedPlayer, _ ) =
                            Player.update playerWithTwoFouls playerWithTwoFouls shotBalls switch runTo
                    in
                    updatedPlayer.previousFouls
                        |> Expect.equal 0

            --
            , fuzz2
                (tuple3 ( player, int, playerSwitch ))
                (intRange 1 150)
                "strictly monotonically increments longestStreak"
              <|
                \( aPlayer, shotBalls, switch ) runTo ->
                    let
                        prevLongestStreak =
                            aPlayer.longestStreak

                        ( updatedPlayer, _ ) =
                            Player.update aPlayer aPlayer shotBalls switch runTo
                    in
                    updatedPlayer.longestStreak
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

                        switch =
                            -- TODO fuzz me
                            No

                        ( updatedPlayer, _ ) =
                            Player.update aPlayer aPlayer shotBalls switch runTo
                    in
                    updatedPlayer.currentStreak
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

                        switch =
                            No

                        ( updatedPlayer, _ ) =
                            Player.update aPlayer aPlayer shotBalls switch runTo
                    in
                    updatedPlayer.currentStreak
                        |> Expect.equal (prevCurrentStreak + shotBalls)

            --
            , fuzz3
                (tuple3 ( leftPlayer, rightPlayer, switchReason ))
                int
                (intRange 1 150)
                "resets currentStreak after switchPlayer"
              <|
                \( left, right, reason ) shotBalls runTo ->
                    let
                        switch =
                            Yes reason

                        ( updatedPlayer, _ ) =
                            Player.update left right shotBalls switch runTo
                    in
                    updatedPlayer.currentStreak
                        |> Expect.equal 0
            ]
        ]
