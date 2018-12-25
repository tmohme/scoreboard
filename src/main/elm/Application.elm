module Application exposing (Event(..), PlayerId(..), nameOf)


type PlayerId
    = Left
    | Right


type Event
    = EntranceExit PlayerId Int


nameOf : PlayerId -> String
nameOf playerId =
    case playerId of
        Left ->
            "Left"

        Right ->
            "Right"
