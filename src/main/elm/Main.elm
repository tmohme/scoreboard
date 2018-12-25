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
    , game : Game.Model
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { page = Entrance
      , entrance = Entrance.init
      , game = Game.init
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

                ( page, maybeBreaker, maybeRunTo ) =
                    case entranceMsg of
                        Entrance.Exit applicationEvent ->
                            let
                                ( playerSelection, runTo ) =
                                    case applicationEvent of
                                        Application.EntranceExit p r ->
                                            ( p, r )
                            in
                            ( Game, Just playerSelection, Just runTo )

                        _ ->
                            ( Entrance, Nothing, Nothing )
            in
            ( { model
                | page = page
                , entrance = updatedEntranceModel
                , game = Game.start model.game maybeBreaker maybeRunTo
              }
            , Cmd.none
            )

        GameMsg gameMsg ->
            let
                updatedGameModel =
                    Game.update gameMsg model.game
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
    css "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.1/css/bulma.min.css"


viewHeader : Html Msg
viewHeader =
    nav [ class "level" ]
        [ div [ class "level-item" ] [ text "left" ]
        , div [ class "level-item" ] [ text "14-1 Scoreboard" ]
        , div [ class "level-item" ] [ text "right" ]
        ]


viewBody : Model -> Html Msg
viewBody model =
    case model.page of
        Entrance ->
            Html.map EntranceMsg (Entrance.view model.entrance)

        Game ->
            Html.map GameMsg (Game.view model.game)


view : Model -> Html Msg
view model =
    div []
        [ bulma
        , viewHeader
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
