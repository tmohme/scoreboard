module Application exposing (Event(..), PlayerSelection(..))


type PlayerSelection
    = Left
    | Right


type Event
    = EntranceExit PlayerSelection Int
