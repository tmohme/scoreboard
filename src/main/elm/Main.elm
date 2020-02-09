module Main exposing (Model, Msg(..), Page(..), bulma, css, init, main)

-- import Debug exposing (log)

import Application
import Browser
import Entrance
import Game
import Html exposing (..)
import Html.Attributes exposing (..)
import Ports


type Page
    = Entrance
    | Game


type Msg
    = EntranceMsg Entrance.Msg
    | GameMsg Game.Msg


type alias Model =
    -- TODO get rid of the attribute interdependencies
    { page : Page
    , entrance : Entrance.Model
    , game : Maybe Game.Model
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { page = Entrance
      , entrance = Entrance.init
      , game = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EntranceMsg entranceMsg ->
            let
                updatedEntranceModel =
                    Entrance.update entranceMsg model.entrance

                ( page, mayBeGame ) =
                    case entranceMsg of
                        -- TODO This is fishy
                        Entrance.Exit (Application.EntranceExit gameConfig) ->
                            ( Game, Just <| Game.init gameConfig )

                        _ ->
                            ( Entrance, Nothing )
            in
            ( { model
                | page = page
                , entrance = updatedEntranceModel
                , game = mayBeGame
              }
            , Cmd.none
            )

        GameMsg gameMsg ->
            let
                ( page, mayBeGame, cmd ) =
                    case gameMsg of
                        -- TODO This is fishy
                        Game.ExitGame ->
                            ( Entrance, Nothing, Cmd.none )

                        _ ->
                            case model.game of
                                Just aGame ->
                                    let
                                        ( newGameModel, aCmd ) =
                                            Game.update gameMsg aGame
                                    in
                                    ( Game, Just newGameModel, Cmd.map GameMsg aCmd )

                                Nothing ->
                                    ( Game, Nothing, Cmd.none )
            in
            ( { model
                | page = page
                , game = mayBeGame
              }
            , cmd
            )


css : String -> Html msg
css path =
    node "link" [ rel "stylesheet", href path ] []


bulma : Html msg
bulma =
    css "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.5/css/bulma.min.css"


viewBody : Model -> Html Msg
viewBody model =
    case model.page of
        Entrance ->
            Html.map EntranceMsg (Entrance.view model.entrance)

        Game ->
            case model.game of
                Just aGame ->
                    Html.map GameMsg (Game.view aGame)

                Nothing ->
                    div [] []


view : Model -> Html Msg
view model =
    div []
        [ bulma
        , viewBody model
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.isFullscreen toGameMsg


toGameMsg : Bool -> Msg
toGameMsg bool =
    GameMsg (Game.IsFullscreen bool)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
