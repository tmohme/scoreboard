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
import Player exposing (Player, PlayerSwitch(..), SwitchReason(..))


type Msg
    = BallsLeftOnTable Int
    | ToggleFoul
    | ShowLog
    | CloseLog
    | ExitGame


type Modal
    = None
    | WinnerModal App.PlayerId
    | LogModal


type alias State =
    { left : Player
    , right : Player
    , shooting : Player
    , ballsOnTable : Int
    , switchReason : SwitchReason
    }


type alias Model =
    { state : State
    , history : List State
    , runTo : Int
    , winner : Maybe Player
    , modal : Modal
    }


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
init gameConfig =
    let
        left =
            Player.create App.Left

        right =
            Player.create App.Right
    in
    { state =
        { left = left
        , right = right
        , shooting = mapToPlayer gameConfig.breakingPlayerId left right
        , ballsOnTable = fullRack
        , switchReason = Miss
        }
    , history = []
    , runTo = gameConfig.runTo
    , winner = Nothing
    , modal = None
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


handleFoulToggle : Model -> Model
handleFoulToggle model =
    let
        switchReason =
            case model.state.switchReason of
                Miss ->
                    Foul

                Foul ->
                    Miss

        oldState =
            model.state

        newState =
            { oldState | switchReason = switchReason }
    in
    { model | state = newState }


handleBallsLeftOnTable : Int -> Model -> Model
handleBallsLeftOnTable remainingBalls model =
    let
        -- TODO special handling for break fouls
        shotBalls =
            model.state.ballsOnTable - remainingBalls

        -- TODO replace 'gameFinished' flag by an ADT properly modeling the Game state (Break, Running, Finished)
        gameFinished =
            (model.state.shooting.points + shotBalls) >= model.runTo

        playerSwitch =
            if model.state.switchReason == Foul then
                Yes Foul

            else if gameFinished || (remainingBalls > 1) || (model.state.ballsOnTable == remainingBalls) then
                Yes model.state.switchReason

            else
                No

        ( updatedLeft, leftTripleFoul ) =
            -- TODO can't we simply update just the shooting player? . . . Resetting his streak etc. after computing the other values?
            Player.update model.state.left model.state.shooting shotBalls playerSwitch model.runTo

        ( updatedRight, rightTripleFoul ) =
            Player.update model.state.right model.state.shooting shotBalls playerSwitch model.runTo

        ballsToContinueWith =
            if (remainingBalls == 1) || leftTripleFoul || rightTripleFoul then
                fullRack

            else
                remainingBalls

        shootingNext =
            determineShootingNext
                model.state.shooting.id
                playerSwitch
                updatedLeft
                updatedRight

        winner =
            determineWinner model.runTo updatedLeft updatedRight

        modal =
            case winner of
                Nothing ->
                    None

                Just player ->
                    WinnerModal player.id

        newHistory =
            model.state :: model.history
    in
    { model
        | state =
            { left = updatedLeft
            , right = updatedRight
            , ballsOnTable = ballsToContinueWith
            , shooting = shootingNext
            , switchReason = Miss
            }
        , history = newHistory
        , winner = winner
        , modal = modal
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleFoul ->
            handleFoulToggle model

        BallsLeftOnTable remainingBalls ->
            handleBallsLeftOnTable remainingBalls model

        -- TODO implement me
        ShowLog ->
            { model | modal = LogModal }

        -- TODO implement me
        CloseLog ->
            { model | modal = None }

        ExitGame ->
            model


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


viewLogModalDialog : Html Msg
viewLogModalDialog =
    div [ class "modal is-active", attribute "aria-label" "Modal title" ]
        [ div [ class "modal-background", onClick CloseLog ]
            []
        , div [ class "modal-card" ]
            [ Html.form [ action "", Html.Events.custom "submit" (Json.Decode.succeed { message = CloseLog, preventDefault = True, stopPropagation = True }) ]
                [ Html.header
                    [ class "modal-card-head" ]
                    [ p [ class "modal-card-title" ]
                        [ text "Spielverlauf" ]
                    , button [ class "delete", onClick CloseLog, attribute "aria-label" "close" ]
                        []
                    ]
                , section [ class "modal-card-body" ]
                    [-- TODO add content
                    ]
                , footer [ class "modal-card-foot" ]
                    [ button [ type_ "button", class "button is-primary", onClick CloseLog, attribute "aria-label" "OK" ]
                        [ text "OK" ]
                    ]
                ]
            ]
        ]


viewWinnerModalDialog : App.PlayerId -> Html Msg
viewWinnerModalDialog playerId =
    div [ class "modal is-active", attribute "aria-label" "Modal title" ]
        [ div [ class "modal-background", onClick ExitGame ]
            []
        , div [ class "modal-card" ]
            [ Html.form [ action "", Html.Events.custom "submit" (Json.Decode.succeed { message = ExitGame, preventDefault = True, stopPropagation = True }) ]
                [ Html.header
                    [ class "modal-card-head" ]
                    [ p [ class "modal-card-title" ]
                        [ text "Gewinner" ]
                    , button [ class "delete", onClick ExitGame, attribute "aria-label" "close" ]
                        []
                    ]
                , section [ class "modal-card-body" ]
                    [ div [ class "field" ]
                        [ label [ class "label" ] [ text "Der Gewinner ist" ]
                        , text (App.nameOf playerId)
                        ]
                    ]
                , footer [ class "modal-card-foot" ]
                    [ button [ type_ "button", class "button is-primary", onClick ExitGame, attribute "aria-label" "OK" ]
                        [ text "OK" ]
                    ]
                ]
            ]
        ]


viewModalDialog : Modal -> Html Msg
viewModalDialog modal =
    case modal of
        WinnerModal winnerId ->
            viewWinnerModalDialog winnerId

        LogModal ->
            viewLogModalDialog

        None ->
            text ""


view : Model -> Html Msg
view model =
    let
        isLeftShooting =
            model.state.shooting == model.state.left

        isRightShooting =
            model.state.shooting == model.state.right

        max =
            model.state.ballsOnTable

        latentFoul =
            if model.state.switchReason == Foul then
                "is-warning"

            else
                ""
    in
    div []
        [ div [ class "columns" ]
            [ div [ class "column is-two-fifth has-text-centered" ]
                [ div [] [ text model.state.left.name ]
                , div [] [ Player.view model.state.left isLeftShooting ]
                ]
            , div [ class "column is-one-fifth has-text-centered is-vertical" ]
                [ div [] [ text "14-1 Scoreboard" ]
                , div [ class "tile is-ancestor is-marginless is-vertical" ]
                    -- TODO make buttons functional
                    [ button [ class "button tile", disabled True ] [ text "RunTo" ]
                    , button [ class "button tile", disabled True ] [ text "Pause / Weiter" ]
                    , button [ class "button tile", onClick ShowLog ] [ text "Log" ]
                    , button [ class "button tile", onClick ExitGame ] [ text "Ende" ] -- TODO add confirmation dialog
                    ]
                ]
            , div [ class "column is-two-fifth has-text-centered" ]
                [ div [] [ text model.state.right.name ]
                , div [] [ Player.view model.state.right isRightShooting ]
                ]
            ]
        , nav [ class "level" ]
            [ div [ class "level-item" ]
                [ button [ class "button is-large", class latentFoul, onClick ToggleFoul ] [ text "Foul" ] ]
            ]
        , div
            [ class "tile is-ancestor" ]
            (List.range
                1
                fullRack
                |> List.map (\n -> viewBall max n)
            )
        , viewModalDialog model.modal
        ]
