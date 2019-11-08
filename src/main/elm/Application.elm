module Application exposing (Event(..), GameConfig, PlayerId(..), nameOf)


type PlayerId
    = Left
    | Right


type alias GameConfig =
    { breakingPlayerId : PlayerId, runTo : Int }


type Event
    = EntranceExit GameConfig


nameOf : PlayerId -> String
nameOf playerId =
    case playerId of
        Left ->
            "Left"

        Right ->
            "Right"
