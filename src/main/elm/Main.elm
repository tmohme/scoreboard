module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (..)
import Json.Decode


type Page
    = Entrance
    | Game


type Msg
    = LeftBreak
    | RightBreak
    | RunToInput String
    | SetRunTo
    | ToggleRunTo
    | BallsLeft Int


type alias Model =
    { runTo : Maybe Int
    , runToBuffer : Maybe Int
    , isPopUpActive : Bool
    , page : Page
    , left : Player
    , right : Player
    , shooting : Maybe Player
    , ballsLeft : Int
    , winner : Maybe Player
    }


type PlayerId
    = Left
    | Right


type alias Player =
    { id : PlayerId
    , points : Int
    , innings : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { runTo = Nothing
      , runToBuffer = Just 80
      , isPopUpActive = False
      , page = Entrance
      , left = Player Left 0 0
      , right = Player Right 0 0
      , shooting = Nothing
      , ballsLeft = 15
      , winner = Nothing
      }
    , Cmd.none
    )


canBreak : Model -> Bool
canBreak model =
    isJust model.runTo


updatedPlayer : Player -> Maybe Player -> Int -> Int -> Player
updatedPlayer player shooting shotBalls inningIncrement =
    case shooting of
        Just someone ->
            if (someone == player) then
                { someone
                    | points = someone.points + shotBalls
                    , innings = someone.innings + inningIncrement
                }
            else
                player

        Nothing ->
            player


determineShootingNext : Maybe Player -> Bool -> Player -> Player -> Maybe Player
determineShootingNext shootingPrevious playerSwitch left right =
    if (playerSwitch) then
        case shootingPrevious of
            Just player ->
                if (player == left) then
                    Just right
                else
                    Just left

            Nothing ->
                Nothing
    else
        case shootingPrevious of
            Just player ->
                if (player == left) then
                    Just left
                else
                    Just right

            Nothing ->
                Nothing


determineWinner : Maybe Int -> Player -> Player -> Maybe Player
determineWinner runTo left right =
    case runTo of
        Just runTo ->
            if (left.points >= runTo) then
                Just left
            else if (right.points >= runTo) then
                Just right
            else
                Nothing

        Nothing ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LeftBreak ->
            let
                player =
                    model.left
            in
                ( { model | page = Game, shooting = Just player, left = player }, Cmd.none )

        RightBreak ->
            let
                player =
                    model.right
            in
                ( { model | page = Game, shooting = Just player, right = player }, Cmd.none )

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

        BallsLeft n ->
            let
                shotBalls =
                    model.ballsLeft - n

                playerSwitch =
                    ((n > 1) || (model.ballsLeft == 1))

                inningIncrement =
                    case playerSwitch of
                        True ->
                            1

                        False ->
                            0

                left =
                    updatedPlayer model.left model.shooting shotBalls inningIncrement

                right =
                    updatedPlayer model.right model.shooting shotBalls inningIncrement

                ballsLeft =
                    if (n == 1) then
                        15
                    else
                        n

                shootingNext =
                    determineShootingNext model.shooting playerSwitch left right

                winner =
                    determineWinner model.runTo left right
            in
                ( { model | ballsLeft = ballsLeft, left = left, right = right, shooting = shootingNext }
                , Cmd.none
                )


css : String -> Html msg
css path =
    node "link" [ rel "stylesheet", href path ] []


bulma : Html msg
bulma =
    css "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.0/css/bulma.min.css"


viewHeader : Html Msg
viewHeader =
    nav [ class "level" ]
        [ div [ class "level-item" ] [ text "left" ]
        , div [ class "level-item" ] [ text "14-1 Scoreboard" ]
        , div [ class "level-item" ] [ text "right" ]
        ]


breakButton : Model -> Msg -> Html Msg
breakButton model msg =
    button [ type_ "button", class "button", disabled (not (canBreak model)), onClick msg ]
        [ text "Break?" ]


runTo : Html Msg
runTo =
    button [ onClick ToggleRunTo, class "button" ]
        [ text "run to ..." ]


viewRunToModalDialog : Model -> Html Msg
viewRunToModalDialog model =
    let
        runToValue =
            Maybe.map (\v -> toString v) model.runToBuffer |> Maybe.withDefault ""
    in
        div [ class "modal is-active", attribute "aria-label" "Modal title" ]
            [ div [ class "modal-background", onClick ToggleRunTo ]
                []
            , div [ class "modal-card" ]
                [ Html.form [ action "", onWithOptions "submit" { preventDefault = True, stopPropagation = True } (Json.Decode.succeed SetRunTo) ]
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


viewEntrance : Model -> Html Msg
viewEntrance model =
    nav [ class "level" ]
        [ div [ class "level-item has-test-cenered" ]
            [ breakButton model LeftBreak ]
        , div [ class "level-item has-test-cenered" ]
            [ runTo ]
        , div [ class "level-item has-test-cenered" ]
            [ breakButton model RightBreak ]
        , if model.isPopUpActive then
            viewRunToModalDialog model
          else
            text ""
        ]


viewPlayer : Player -> Bool -> Html Msg
viewPlayer player isShooting =
    let
        style =
            case isShooting of
                True ->
                    "has-background-primary"

                False ->
                    ""

        totalAvg =
            if (player.innings > 0) then
                (round ((toFloat player.points / toFloat player.innings) * 10.0) |> toFloat) / 10.0
            else
                0.0
    in
        div []
            [ p [ class ("big-auto-size" ++ " " ++ style) ] [ text (player.points |> toString) ]
            , div [ class ("level" ++ " " ++ style) ]
                [ div [ class "level-item has-text-centered" ]
                    [ p [ class "heading" ] [ text "AN" ]
                    , p [ class "title" ] [ text (player.innings |> toString) ]
                    ]
                , div [ class "level-item has-text-centered" ]
                    [ p [ class "heading" ] [ text "GD" ]
                    , p [ class "title" ] [ text (totalAvg |> toString) ]
                    ]
                , div [ class "level-item has-text-centered" ]
                    [ p [ class "heading" ] [ text "HA" ]
                    , p [ class "title" ] [ text "?" ]
                    ]
                , div [ class "level-item has-text-centered" ]
                    [ p [ class "heading" ] [ text "Fouls" ]
                    , p [ class "title" ] [ text "?" ]
                    ]
                ]
            ]


viewBall : Int -> Int -> Html Msg
viewBall max n =
    let
        maybeHidden =
            if (n > max) then
                Html.Attributes.hidden True
            else
                Html.Attributes.hidden False
    in
        div [ maybeHidden ]
            [ img [ alt (toString n), src ("img/" ++ (toString n) ++ "B.svg"), onClick (BallsLeft n) ] []
            ]


viewGame : Model -> Html Msg
viewGame model =
    let
        isLeftShooting =
            (model.shooting == Just model.left)

        isRightShooting =
            (model.shooting == Just model.right)

        max =
            model.ballsLeft
    in
        div
            []
            [ div
                [ class "columns" ]
                [ div [ class "column is-two-fifth is-centered has-text-centered" ] [ viewPlayer model.left isLeftShooting ]
                , div [ class "column is-one-fifth is-centered  has-text-centered" ]
                    [ button [ class "button", disabled True ] [ text "Vollbild" ]
                    , button [ class "button", disabled True ] [ text "RunTo" ]
                    , button [ class "button", disabled True ] [ text "Pause / Weiter" ]
                    , button [ class "button", disabled True ] [ text "Log / Undo" ]
                    , button [ class "button", disabled True ] [ text "Ende" ]
                    ]
                , div [ class "column is-two-fifth is-centered has-text-centered" ] [ viewPlayer model.right isRightShooting ]
                ]
            , footer [ class "footer" ]
                [ div [ class "container" ]
                    [ viewBall max 1
                    , viewBall max 2
                    , viewBall max 3
                    , viewBall max 4
                    , viewBall max 5
                    , viewBall max 6
                    , viewBall max 7
                    , viewBall max 8
                    , viewBall max 9
                    , viewBall max 10
                    , viewBall max 11
                    , viewBall max 12
                    , viewBall max 13
                    , viewBall max 14
                    , viewBall max 15
                    ]
                ]
            ]


viewBody : Model -> Html Msg
viewBody model =
    case model.page of
        Entrance ->
            viewEntrance model

        Game ->
            viewGame model


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


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
