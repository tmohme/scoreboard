module Main exposing (Model, Msg(..), Page(..), bulma, css, init, main)

-- import Debug exposing (log)

import Application
import Browser
import Entrance
import Game
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (..)


type Page
    = Entrance
    | Game


type Msg
    = EntranceMsg Entrance.Msg
    | GameMsg Game.Msg


type alias Model =
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
                updatedGameModel =
                    case model.game of
                        Just aGame ->
                            Just <| Game.update gameMsg aGame

                        Nothing ->
                            Nothing
            in
            ( { model
                | game = updatedGameModel
              }
            , Cmd.none
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
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
