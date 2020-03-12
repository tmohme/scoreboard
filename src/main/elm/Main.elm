module Main exposing (..)

import Application
import Browser
import Browser.Navigation as Nav
import Entrance
import Game
import Html exposing (..)
import Html.Attributes exposing (..)
import Ports
import Route exposing (Route, href)
import Session exposing (Session)
import Url exposing (Url)


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotEntranceMsg Entrance.Msg
    | GotGameMsg Game.Msg


type Model
    = Entrance Entrance.Model
    | Game Game.Model


init : flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( entrance, msg ) =
            Entrance.init (Session.init url key)
    in
    ( Entrance entrance
    , Cmd.map GotEntranceMsg msg
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.replaceUrl (Session.navKey (toSession model)) (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( GotEntranceMsg subMsg, Entrance entrance ) ->
            Entrance.update subMsg entrance
                |> updateWith Entrance GotEntranceMsg

        ( GotGameMsg subMsg, Game game ) ->
            Game.update subMsg game
                |> updateWith Game GotGameMsg

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    case route of
        Nothing ->
            -- TODO invalid Route - sensible handling?
            ( model, Cmd.none )

        Just Route.Entrance ->
            Entrance.init (toSession model)
                |> updateWith Entrance GotEntranceMsg

        Just (Route.Game config) ->
            Game.init (toSession model) config
                |> updateWith Game GotGameMsg


toSession : Model -> Session
toSession model =
    case model of
        Entrance entrance ->
            Entrance.session entrance

        Game game ->
            Game.session game


navKey : Model -> Nav.Key
navKey model =
    toSession model |> Session.navKey


toConfig : Model -> Application.GameConfig
toConfig model =
    case model of
        Entrance entrance ->
            Entrance.toConfig entrance

        Game game ->
            Game.toConfig game


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


css : String -> Html msg
css path =
    node "link" [ rel "stylesheet", Html.Attributes.href path ] []


bulma : Html msg
bulma =
    css "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.5/css/bulma.min.css"


viewBody : Model -> Html Msg
viewBody model =
    case model of
        Entrance entrance ->
            Entrance.view entrance |> Html.map GotEntranceMsg

        Game game ->
            Game.view game |> Html.map GotGameMsg


view : Model -> Browser.Document Msg
view model =
    { title = "Scoreboard"
    , body =
        [ div []
            [ bulma
            , viewBody model
            ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.isFullscreen toGameMsg


toGameMsg : Bool -> Msg
toGameMsg bool =
    GotGameMsg (Game.IsFullscreen bool)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
