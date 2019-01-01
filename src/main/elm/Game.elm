module Game exposing
    ( Model
    , Msg(..)
    , determineShootingNext
    , determineWinner
    , init
    , update
    , view
    )

import Application as App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Player exposing (Player, PlayerSwitch(..), SwitchReason(..), view)


type Msg
    = BallsLeftOnTable Int
    | WinnerShown


type alias Model =
    { left : Player
    , right : Player
    , shooting : Player
    , ballsLeftOnTable : Int
    , runTo : Int
    , winner : Maybe Player
    , showWinner : ShowWinner
    }


type ShowWinner
    = NotYet
    | ShowWinner App.PlayerId
    | AlreadyShown


fullRack =
    15


mapToPlayer : App.PlayerId -> Player -> Player -> Player
mapToPlayer playerId left right =
    case playerId of
        App.Left ->
            left

        App.Right ->
            right


init : App.GameConfig -> Model
init config =
    let
        left =
            Player.create App.Left

        right =
            Player.create App.Right
    in
    { left = left
    , right = right
    , shooting = mapToPlayer config.playerId left right
    , ballsLeftOnTable = fullRack
    , runTo = config.runTo
    , winner = Nothing
    , showWinner = NotYet
    }


determineShootingNext : App.PlayerId -> PlayerSwitch -> Player -> Player -> Player
determineShootingNext shootingPrevious playerSwitch left right =
    case playerSwitch of
        Yes _ ->
            if shootingPrevious == left.id then
                right

            else
                left

        No ->
            if shootingPrevious == left.id then
                left

            else
                right


determineWinner : Int -> Player -> Player -> Maybe Player
determineWinner runToPoints left right =
    if left.points >= runToPoints then
        Just left

    else if right.points >= runToPoints then
        Just right

    else
        Nothing


update : Msg -> Model -> Model
update msg model =
    case msg of
        BallsLeftOnTable n ->
            let
                shotBalls =
                    model.ballsLeftOnTable - n

                gameFinished =
                    (model.shooting.points + shotBalls) >= model.runTo

                playerSwitch =
                    -- TODO get rid of 'gameFinished'
                    if gameFinished || (n > 1) || (model.ballsLeftOnTable == n) then
                        Yes Miss

                    else
                        No

                left =
                    -- TODO can't we simply update just the shooting player? . . . Resetting his streak etc. after computing the other values?
                    Player.update model.left model.shooting shotBalls playerSwitch model.runTo

                right =
                    Player.update model.right model.shooting shotBalls playerSwitch model.runTo

                ballsOnTable =
                    if n == 1 then
                        fullRack

                    else
                        n

                shootingNext =
                    determineShootingNext
                        model.shooting.id
                        playerSwitch
                        left
                        right

                winner =
                    determineWinner model.runTo left right

                showWinner =
                    case ( winner, model.showWinner ) of
                        ( Nothing, _ ) ->
                            NotYet

                        ( Just player, NotYet ) ->
                            ShowWinner player.id

                        ( Just player, _ ) ->
                            AlreadyShown
            in
            { model
                | ballsLeftOnTable = ballsOnTable
                , left = left
                , right = right
                , shooting = shootingNext
                , winner = winner
                , showWinner = showWinner
            }

        WinnerShown ->
            { model | showWinner = AlreadyShown }


viewBall : Int -> Int -> Html Msg
viewBall max n =
    let
        visibility =
            if n > max then
                " is-invisible"

            else
                ""
    in
    button [ class <| "button tile" ++ visibility ]
        [ img
            [ alt (String.fromInt n)
            , src <| "img/" ++ String.fromInt n ++ "B.svg"
            , onClick (BallsLeftOnTable n)
            ]
            []
        ]


viewWinnerModalDialog : App.PlayerId -> Html Msg
viewWinnerModalDialog playerId =
    div [ class "modal is-active", attribute "aria-label" "Modal title" ]
        [ div [ class "modal-background", onClick WinnerShown ]
            []
        , div [ class "modal-card" ]
            [ Html.form [ action "", Html.Events.custom "submit" (Json.Decode.succeed { message = WinnerShown, preventDefault = True, stopPropagation = True }) ]
                [ Html.header
                    [ class "modal-card-head" ]
                    [ p [ class "modal-card-title" ]
                        [ text "Gewinner" ]
                    , button [ class "delete", onClick WinnerShown, attribute "aria-label" "close" ]
                        []
                    ]
                , section [ class "modal-card-body" ]
                    [ div [ class "field" ]
                        [ label [ class "label" ] [ text "Der Gewinner ist" ]
                        , text (App.nameOf playerId)
                        ]
                    ]
                , footer [ class "modal-card-foot" ]
                    [ button [ type_ "button", class "button is-primary", onClick WinnerShown, attribute "aria-label" "OK" ]
                        [ text "OK" ]
                    ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    let
        isLeftShooting =
            model.shooting == model.left

        isRightShooting =
            model.shooting == model.right

        max =
            model.ballsLeftOnTable
    in
    div
        []
        [ div
            [ class "columns" ]
            [ div [ class "column is-two-fifth is-centered has-text-centered" ]
                [ Player.view model.left isLeftShooting ]
            , div [ class "column is-one-fifth is-centered tile is-ancestor is-vertical" ]
                [ button [ class "button tile", disabled True ] [ text "Vollbild" ]
                , button [ class "button tile", disabled True ] [ text "RunTo" ]
                , button [ class "button tile", disabled True ] [ text "Pause / Weiter" ]
                , button [ class "button tile", disabled True ] [ text "Log / Undo" ]
                , button [ class "button tile", disabled True ] [ text "Ende" ]
                ]
            , div [ class "column is-two-fifth is-centered has-text-centered" ]
                [ Player.view model.right isRightShooting ]
            ]
        , div
            [ class "tile is-ancestor" ]
            (List.range
                1
                fullRack
                |> List.map (\n -> viewBall max n)
            )
        , case model.showWinner of
            ShowWinner winnerId ->
                viewWinnerModalDialog winnerId

            _ ->
                text ""
        ]
