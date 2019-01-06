module Game exposing
    ( Model
    , Msg(..)
    , determineShootingNext
    , determineWinner
    , fullRack
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
    | ToggleSwitchReason
    | WinnerShown


type alias Model =
    { left : Player
    , right : Player
    , shooting : Player
    , ballsOnTable : Int
    , runTo : Int
    , winner : Maybe Player
    , showWinner : ShowWinner
    , switchReason : SwitchReason
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
    , ballsOnTable = fullRack
    , runTo = config.runTo
    , winner = Nothing
    , showWinner = NotYet
    , switchReason = Miss
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
        ToggleSwitchReason ->
            let
                reason =
                    case model.switchReason of
                        Miss ->
                            Foul

                        Foul ->
                            Miss
            in
            { model | switchReason = reason }

        BallsLeftOnTable remainingBalls ->
            let
                -- TODO special handling for break fouls
                shotBalls =
                    model.ballsOnTable - remainingBalls

                -- TODO replace 'gameFinished' flag by an ADT properly modeling the Game state (Break, Running, Finished)
                gameFinished =
                    (model.shooting.points + shotBalls) >= model.runTo

                playerSwitch =
                    if model.switchReason == Foul then
                        Yes Foul

                    else if gameFinished || (remainingBalls > 1) || (model.ballsOnTable == remainingBalls) then
                        Yes model.switchReason

                    else
                        No

                ( left, leftTripleFoul ) =
                    -- TODO can't we simply update just the shooting player? . . . Resetting his streak etc. after computing the other values?
                    Player.update model.left model.shooting shotBalls playerSwitch model.runTo

                ( right, rightTripleFoul ) =
                    Player.update model.right model.shooting shotBalls playerSwitch model.runTo

                ballsToContinueWith =
                    if (remainingBalls == 1) || leftTripleFoul || rightTripleFoul then
                        fullRack

                    else
                        remainingBalls

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
                | ballsOnTable = ballsToContinueWith
                , left = left
                , right = right
                , shooting = shootingNext
                , winner = winner
                , showWinner = showWinner
                , switchReason = Miss
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
            model.ballsOnTable

        latentFoul =
            if model.switchReason == Foul then
                "is-warning"

            else
                ""
    in
    div
        []
        [ div
            [ class "columns" ]
            [ div [ class "column is-two-fifth is-centered has-text-centered" ]
                [ Player.view model.left isLeftShooting ]
            , nav [ class "column is-one-fifth is-centered tile is-ancestor is-vertical" ]
                [ button [ class "button tile", disabled True ] [ text "Vollbild" ]
                , button [ class "button tile", disabled True ] [ text "RunTo" ]
                , button [ class "button tile", disabled True ] [ text "Pause / Weiter" ]
                , button [ class "button tile", disabled True ] [ text "Log / Undo" ]
                , button [ class "button tile", disabled True ] [ text "Ende" ]
                ]
            , div [ class "column is-two-fifth is-centered has-text-centered" ]
                [ Player.view model.right isRightShooting ]
            ]
        , nav [ class "level" ]
            [ div [ class "level-item" ]
                [ button [ class "button is-large", class latentFoul, onClick ToggleSwitchReason ] [ text "Foul" ] ]
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
