module ApplicationSupport exposing (playerId)

import Application as App
import Fuzz exposing (Fuzzer)
import Player as P


playerId : Fuzzer App.PlayerId
playerId =
    Fuzz.oneOf
        [ Fuzz.constant App.Left
        , Fuzz.constant App.Right
        ]
