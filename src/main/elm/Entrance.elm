module Entrance exposing (Break, Model, Msg(..), init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust)


type Break
    = Undefined
    | Left
    | Right


type alias Model =
    { break : Break
    , runTo : Maybe Int
    , runToBuffer : Maybe Int
    , isSettingRunTo : Bool
    }


type Msg
    = LeftBreak
    | RightBreak
    | RunToInput String
    | SetRunTo
    | ToggleRunTo


init : Model
init =
    { break = Undefined
    , runTo = Nothing
    , runToBuffer = Just 80
    , isSettingRunTo = False
    }


canBreak : Model -> Bool
canBreak model =
    isJust model.runTo


breakButton : Model -> Msg -> Html Msg
breakButton model msg =
    button [ type_ "button", class "button", disabled (not (canBreak model)), onClick msg ]
        [ text "Break?" ]


runToHtml : Html Msg
runToHtml =
    button [ onClick ToggleRunTo, class "button" ]
        [ text "run to ..." ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        LeftBreak ->
            { model | break = Left }

        RightBreak ->
            { model | break = Right }

        RunToInput s ->
            { model | runToBuffer = String.toInt s }

        SetRunTo ->
            { model | isSettingRunTo = False, runTo = model.runToBuffer }

        ToggleRunTo ->
            { model | isSettingRunTo = not model.isSettingRunTo }


view : Model -> Html Msg
view model =
    nav [ class "level" ]
        [ div [ class "level-item has-test-cenered" ]
            [ breakButton model LeftBreak ]
        , div [ class "level-item has-test-cenered" ]
            [ runToHtml ]
        , div [ class "level-item has-test-cenered" ]
            [ breakButton model RightBreak ]
        , if model.isSettingRunTo then
            viewRunToModalDialog model

          else
            text ""
        ]


viewRunToModalDialog : Model -> Html Msg
viewRunToModalDialog model =
    let
        runToValue =
            Maybe.map (\v -> String.fromInt v) model.runToBuffer |> Maybe.withDefault ""
    in
    div [ class "modal is-active", attribute "aria-label" "Modal title" ]
        [ div [ class "modal-background", onClick ToggleRunTo ]
            []
        , div [ class "modal-card" ]
            [ Html.form [ action "", Html.Events.custom "submit" (Json.Decode.succeed { message = SetRunTo, preventDefault = True, stopPropagation = True }) ]
                [ Html.header
                    [ class "modal-card-head" ]
                    [ p [ class "modal-card-title" ]
                        [ text "Spielziel" ]
                    , button [ class "delete", onClick ToggleRunTo, attribute "aria-label" "close" ]
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
