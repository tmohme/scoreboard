module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (..)


type Page
    = Entrance
    | Game


type Msg
    = LeftBreak
    | RightBreak
    | RunToInput String
    | SetRunTo
    | ToggleRunTo


type alias Model =
    { runTo : Maybe Int
    , runToBuffer : Maybe Int
    , isPopUpActive : Bool
    , page : Page
    , left : Player
    , right : Player
    }


type alias Player =
    { points : Int }


init : ( Model, Cmd Msg )
init =
    ( { runTo = Nothing
      , runToBuffer = Just 80
      , isPopUpActive = False
      , page = Entrance
      , left = Player 0
      , right = Player 0
      }
    , Cmd.none
    )


canBreak : Model -> Bool
canBreak model =
    isJust model.runTo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LeftBreak ->
            ( { model | page = Game }, Cmd.none )

        RightBreak ->
            ( { model | page = Game }, Cmd.none )

        RunToInput s ->
            ( { model | runToBuffer = String.toInt s |> Result.toMaybe }
            , Cmd.none
            )

        SetRunTo ->
            ( { model | isPopUpActive = False, runTo = model.runToBuffer }
            , Cmd.none
            )

        ToggleRunTo ->
            ( { model | isPopUpActive = not model.isPopUpActive }
            , Cmd.none
            )


css : String -> Html msg
css path =
    node "link" [ rel "stylesheet", href path ] []


bulma : Html msg
bulma =
    css "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.0/css/bulma.min.css"


header : Html Msg
header =
    text "14-1 Scoreboard"


break : Model -> Msg -> Html Msg
break model msg =
    button [ type_ "button", class "button", disabled (not (canBreak model)), onClick msg ]
        [ text "Break?" ]


runTo : Html Msg
runTo =
    button [ onClick ToggleRunTo, class "button" ]
        [ text "run to ..." ]


renderModal : Model -> Html Msg
renderModal model =
    let
        runToValue =
            Maybe.map (\v -> toString v) model.runToBuffer |> Maybe.withDefault ""
    in
        div [ class "modal is-active", attribute "aria-label" "Modal title" ]
            [ div [ class "modal-background", onClick ToggleRunTo ]
                []
            , div [ class "modal-card" ]
                [ Html.header [ class "modal-card-head" ]
                    [ p [ class "modal-card-title" ]
                        [ text "Spielziel" ]
                    , button [ class "delete", onClick ToggleRunTo, attribute "aria-label" "close" ]
                        []
                    ]
                , section [ class "modal-card-body" ]
                    [ text "Gib dein Spielziel ein (z.B. 125):"
                    , input [ type_ "number", class "input", value runToValue, step "5", onInput RunToInput ] []
                    ]
                , footer [ class "modal-card-foot" ]
                    [ button [ class "button is-primary", onClick SetRunTo, attribute "aria-label" "OK" ]
                        [ text "OK" ]
                    ]
                ]
            ]


viewEntrance : Model -> Html Msg
viewEntrance model =
    div []
        [ break model LeftBreak
        , runTo
        , break model RightBreak
        , if model.isPopUpActive then
            renderModal model
          else
            text ""
        ]


viewPlayer : Player -> Html Msg
viewPlayer p =
    Html.p [] [ text (p.points |> toString) ]


viewGame : Model -> Html Msg
viewGame model =
    div [ class "columns" ]
        [ div [ class "column is-centered has-text-centered" ] [ viewPlayer model.left ]
        , div [ class "column is-centered  has-text-centered is-narrow" ]
            [ button [ class "button" ] [ text "Vollbild" ]
            , button [ class "button" ] [ text "RunTo" ]
            , button [ class "button" ] [ text "Pause / Weiter" ]
            , button [ class "button" ] [ text "Log / Undo" ]
            , button [ class "button" ] [ text "Ende" ]
            ]
        , div [ class "column is-centered has-text-centered" ] [ viewPlayer model.right ]
        ]


body : Model -> Html Msg
body model =
    case model.page of
        Entrance ->
            viewEntrance model

        Game ->
            viewGame model


view : Model -> Html Msg
view model =
    div []
        [ bulma
        , header
        , body model
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
