port module Ports exposing (..)


port requestFullscreen : Bool -> Cmd msg


port isFullscreen : (Bool -> msg) -> Sub msg
