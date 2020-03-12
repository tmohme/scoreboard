module Entrance exposing
    ( Model
    , Msg(..)
    , init
    , session
    , toConfig
    , update
    , view
    )

import Application as App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Route exposing (Route)
import Session exposing (Session)


type alias Model =
    { session : Session
    , runTo : Int
    , runToBuffer : Maybe Int
    , runToModalVisible : Bool
    , config : App.GameConfig
    }


toConfig : Model -> App.GameConfig
toConfig model =
    model.config


session : Model -> Session
session model =
    model.session


type Msg
    = RunToInput String
    | SetRunTo
    | ToggleRunToModal


defaultTarget =
    80


init : Session -> ( Model, Cmd Msg )
init s =
    ( { session = s
      , runTo = defaultTarget
      , runToBuffer = Just defaultTarget
      , runToModalVisible = False
      , config = App.init
      }
    , Cmd.none
    )


breakButton : App.GameConfig -> Html Msg
breakButton config =
    a [ class "button", Route.href (Route.Game config) ] [ text "Break?" ]


runToButton : Model -> Html Msg
runToButton model =
    button [ onClick ToggleRunToModal, class "button" ]
        [ text <| "run to ... (" ++ String.fromInt model.runTo ++ ")" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RunToInput s ->
            ( { model | runToBuffer = String.toInt s }, Cmd.none )

        SetRunTo ->
            ( { model | runToModalVisible = False, runTo = Maybe.withDefault model.runTo model.runToBuffer }, Cmd.none )

        ToggleRunToModal ->
            ( { model | runToModalVisible = not model.runToModalVisible }, Cmd.none )


container : Html Msg -> Html Msg
container content =
    div [ class "container" ] [ content ]


view : Model -> Html Msg
view model =
    div []
        [ div [ class "columns" ]
            [ div [ class "column has-text-centered" ]
                -- TODO get text from players
                -- TODO make it a button that enables name editing
                [ container (text "left")
                , container (breakButton <| App.GameConfig App.Left model.runTo)
                ]
            , div [ class "column has-text-centered" ]
                [ container (text "14-1 Scoreboard")
                , container (runToButton model)
                ]
            , div [ class "column has-text-centered" ]
                -- TODO get text from players
                -- TODO make it a button that enables name editing
                [ container (text "right")
                , container (breakButton <| App.GameConfig App.Right model.runTo)
                ]
            ]
        , viewRunToModal model
        ]


viewRunToModal : Model -> Html Msg
viewRunToModal model =
    let
        runToValue =
            Maybe.map (\v -> String.fromInt v) model.runToBuffer |> Maybe.withDefault ""

        isActive =
            case model.runToModalVisible of
                True ->
                    " is-active"

                False ->
                    ""
    in
    div [ class <| "modal" ++ isActive, attribute "aria-label" "Modal title" ]
        [ div [ class "modal-background", onClick ToggleRunToModal ]
            []
        , div [ class "modal-card" ]
            [ Html.form [ action "", Html.Events.custom "submit" (Json.Decode.succeed { message = SetRunTo, preventDefault = True, stopPropagation = True }) ]
                [ Html.header
                    [ class "modal-card-head" ]
                    [ p [ class "modal-card-title" ]
                        [ text "Spielziel" ]
                    , button [ class "delete", onClick ToggleRunToModal, attribute "aria-label" "close" ]
                        []
                    ]
                , section [ class "modal-card-body" ]
                    [ div [ class "field" ]
                        [ label [ class "label" ] [ text "Gib dein Spielziel ein (z.B. 125):" ]
                        , input [ type_ "number", class "input", value runToValue, step "5", onInput RunToInput ] []
                        ]
                    ]
                , footer [ class "modal-card-foot" ]
                    [ button [ type_ "button", class "button is-primary", onClick SetRunTo, attribute "aria-label" "OK" ]
                        [ text "OK" ]
                    ]
                ]
            ]
        ]
