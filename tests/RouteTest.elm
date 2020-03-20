module RouteTest exposing (..)

import Application as App
import Expect
import Route exposing (Route(..))
import String
import Test exposing (..)
import Url exposing (Url)
import Url.Builder exposing (QueryParameter, int, string)


basePath =
    "something"


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
        [ testUrl basePath Nothing Entrance
        , testUrl (basePath ++ "/game") (Just [ string "break" right, int "runTo" 81 ]) (Game (App.GameConfig App.Right 81))
        , testUrl (basePath ++ "/game") (Just [ int "runTo" 82, string "break" right ]) (Game (App.GameConfig App.Right 82))
        , testUrl (basePath ++ "/game") (Just [ string "break" left ]) (Game { defaultConfig | breakingPlayerId = App.Left })
        , testUrl (basePath ++ "/game") (Just [ string "break" right ]) (Game { defaultConfig | breakingPlayerId = App.Right })
        , testUrl (basePath ++ "/game") (Just [ int "runTo" 83 ]) (Game { defaultConfig | runTo = 83 })
        , testUrl (basePath ++ "/game") (Just []) (Game defaultConfig)
        , testUrl (basePath ++ "/game") Nothing (Game defaultConfig)
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

        baseUrl =
            { protocol = Url.Http
            , host = "anywhere"
            , port_ = Nothing
            , path = basePath
            , query = Nothing
            , fragment = Nothing
            }
    in
    test ("Parsing path: '" ++ path ++ queryString ++ "'") <|
        \() ->
            ( path, query )
                |> toUrl
                |> Route.fromUrl baseUrl
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
