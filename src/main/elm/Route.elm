module Route exposing (Route(..), fromUrl, href, toString)

import Application as App exposing (nameOf)
import Dict
import Html exposing (Attribute)
import Html.Attributes as Attributes
import Url exposing (Url)
import Url.Builder exposing (string)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s)
import Url.Parser.Query as Query


type Route
    = Entrance
    | Game App.GameConfig


type alias QueryData =
    { runTo : Maybe Int
    , playerId : Maybe App.PlayerId
    }


playerIdQueryParser : Query.Parser (Maybe App.PlayerId)
playerIdQueryParser =
    Query.enum "playerId" (Dict.fromList [ ( "Left", App.Left ), ( "Right", App.Right ) ])



{-
   configQuery : Query.Parser App.GameConfig
   configQuery =
       Query.map2 App.GameConfig (Query.int "runTo") playerIdQueryParser


   f : Maybe Int -> Maybe App.PlayerId -> Maybe App.GameConfig
   f runTo playerId =
       Maybe.map2 App.GameConfig runTo playerId
-}


playerIdParser : Parser (App.PlayerId -> a) a
playerIdParser =
    Parser.custom "PLAYER_ID" <|
        \segment ->
            case segment of
                "Left" ->
                    Just App.Left

                "Right" ->
                    Just App.Right

                _ ->
                    Nothing


configParser =
    Parser.map App.GameConfig (playerIdParser </> Parser.int)


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Entrance (s "scoreboard")
        , Parser.map Game (s "scoreboard" </> s "break" </> configParser)
        ]



-- public helpers


href : Route -> Attribute msg
href targetRoute =
    Attributes.href (toString targetRoute)


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url


toString : Route -> String
toString route =
    case route of
        Entrance ->
            ""

        Game config ->
            Url.Builder.relative
                [ "break", nameOf config.breakingPlayerId, String.fromInt config.runTo ]
                []
