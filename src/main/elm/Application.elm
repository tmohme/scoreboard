module Application exposing (Event(..), GameConfig, PlayerId(..), init, nameOf)


type PlayerId
    = Left
    | Right


type alias GameConfig =
    { breakingPlayerId : PlayerId
    , runTo : Int
    }


type Event
    = EntranceExit GameConfig


init : GameConfig
init =
    { breakingPlayerId = Left
    , runTo = 50
    }


nameOf : PlayerId -> String
nameOf playerId =
    case playerId of
        Left ->
            "Left"

        Right ->
            "Right"
