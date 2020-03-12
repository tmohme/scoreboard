module RouteTest exposing (..)

import Application as App
import Expect
import Route exposing (Route(..))
import Test exposing (..)
import Url exposing (Url)


fromUrl : Test
fromUrl =
    describe "Route.fromUrl"
        [ testUrl "scoreboard/" Entrance
        , testUrl "scoreboard/break/Left/80" (Game (App.GameConfig App.Left 80))
        ]



-- HELPERS


testUrl : String -> Route -> Test
testUrl path route =
    test ("Parsing path: \"" ++ path ++ "\"") <|
        \() ->
            path
                |> toUrl
                |> Route.fromUrl
                |> Expect.equal (Just route)


toUrl : String -> Url
toUrl path =
    { protocol = Url.Http
    , host = "foo.com"
    , port_ = Nothing
    , path = path
    , query = Nothing
    , fragment = Nothing
    }
