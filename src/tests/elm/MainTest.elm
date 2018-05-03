module MainTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string, intRange)
import Test exposing (..)
import Main exposing (..)


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
        ]
