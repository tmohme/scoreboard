module Route exposing (Route(..), fromUrl, href, toString)

import Application as App
import Dict
import Html exposing (Attribute)
import Html.Attributes as Attributes
import Url exposing (Url)
import Url.Builder exposing (int, string)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s)
import Url.Parser.Query as Query


type Route
    = Entrance
    | Game App.GameConfig


type alias QueryData =
    { playerId : Maybe App.PlayerId
    , runTo : Maybe Int
    }


breakingPlayerIdQueryParser : Query.Parser (Maybe App.PlayerId)
breakingPlayerIdQueryParser =
    Query.enum "break"
        (Dict.fromList
            [ ( App.nameOf App.Left, App.Left )
            , ( App.nameOf App.Right, App.Right )
            ]
        )


configQueryParser : Query.Parser QueryData
configQueryParser =
    Query.map2 QueryData breakingPlayerIdQueryParser (Query.int "runTo")


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Entrance Parser.top
        , Parser.map queryDataToGame (s "game" <?> configQueryParser)
        ]


queryDataToGame : QueryData -> Route
queryDataToGame qd =
    let
        defaultConfig =
            App.init

        cfg =
            case ( qd.playerId, qd.runTo ) of
                ( Just playerId, Just runTo ) ->
                    App.GameConfig playerId runTo

                ( Just playerId, _ ) ->
                    { defaultConfig | breakingPlayerId = playerId }

                ( _, Just runTo ) ->
                    { defaultConfig | runTo = runTo }

                ( _, _ ) ->
                    defaultConfig
    in
    Game cfg



-- public helpers


href : Route -> Attribute msg
href targetRoute =
    Attributes.href (toString targetRoute)


fromUrl : Url -> Url -> Maybe Route
fromUrl baseUrl url =
    Debug.log "Route.fromUrl" <|
        Parser.parse parser (dropBasePath baseUrl url)


dropBasePath : Url -> Url -> Url
dropBasePath baseUrl url =
    let
        baseSegments =
            String.split "/" baseUrl.path

        segments =
            String.split "/" url.path

        numberOfCommonParents =
            List.map2 (\member1 member2 -> member1 == member2) segments baseSegments
                |> List.filter identity
                |> List.length

        effectiveSegments =
            List.drop numberOfCommonParents segments
    in
    { url
        | path = String.join "/" effectiveSegments
    }


toString : Route -> String
toString route =
    case route of
        Entrance ->
            ""

        Game config ->
            Url.Builder.relative
                [ "game" ]
                [ string "break" (App.nameOf config.breakingPlayerId), int "runTo" config.runTo ]
