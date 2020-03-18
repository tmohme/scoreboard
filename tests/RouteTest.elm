module RouteTest exposing (..)

import Application as App
import Expect
import Route exposing (Route(..))
import String
import Test exposing (..)
import Url exposing (Url)
import Url.Builder exposing (QueryParameter, int, string)


fromUrl : Test
fromUrl =
    let
        defaultConfig =
            App.init

        left =
            App.nameOf App.Left

        right =
            App.nameOf App.Right
    in
    describe "Route.fromUrl"
        [ testUrl "scoreboard" Nothing Entrance
        , testUrl "scoreboard/game" (Just [ string "break" right, int "runTo" 81 ]) (Game (App.GameConfig App.Right 81))
        , testUrl "scoreboard/game" (Just [ int "runTo" 82, string "break" right ]) (Game (App.GameConfig App.Right 82))
        , testUrl "scoreboard/game" (Just [ string "break" left ]) (Game { defaultConfig | breakingPlayerId = App.Left })
        , testUrl "scoreboard/game" (Just [ string "break" right ]) (Game { defaultConfig | breakingPlayerId = App.Right })
        , testUrl "scoreboard/game" (Just [ int "runTo" 83 ]) (Game { defaultConfig | runTo = 83 })
        , testUrl "scoreboard/game" (Just []) (Game defaultConfig)
        , testUrl "scoreboard/game" Nothing (Game defaultConfig)
        ]



-- HELPERS


testUrl : String -> Maybe (List QueryParameter) -> Route -> Test
testUrl path query route =
    let
        queryString =
            case query of
                Nothing ->
                    ""

                Just [] ->
                    "?"

                Just params ->
                    Url.Builder.toQuery params
    in
    test ("Parsing path: '" ++ path ++ queryString ++ "'") <|
        \() ->
            ( path, query )
                |> toUrl
                |> Route.fromUrl
                |> Expect.equal (Just route)


toUrl : ( String, Maybe (List QueryParameter) ) -> Url
toUrl ( path, qp ) =
    let
        query =
            case qp of
                Nothing ->
                    Nothing

                Just params ->
                    Just (String.dropLeft 1 (Url.Builder.toQuery params))
    in
    { protocol = Url.Http
    , host = "foobar.com"
    , port_ = Nothing
    , path = path
    , query = query
    , fragment = Nothing
    }
