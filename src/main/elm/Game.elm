module Game exposing
    ( Model
    , Msg(..)
    , State
    , determineShootingNext
    , determineWinner
    , fullRack
    , handleBallsLeftOnTable
    , init
    , session
    , update
    , view
    )

import Application as App
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Player exposing (Player, PlayerSwitch(..), SwitchReason(..))
import Ports
import Route
import Session exposing (Session)
import Url exposing (Url)


type Msg
    = BallsLeftOnTable Int
    | IsFullscreen Bool
    | RequestFullscreen Bool
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
    { session : Session
    , state : State
    , history : List State
    , runTo : Int
    , winner : Maybe Player
    , modal : Modal
    , isFullscreen : Bool
    , config : App.GameConfig
    }


session : Model -> Session
session model =
    model.session


fullRack =
    15


mapToPlayer : App.PlayerId -> Player -> Player -> Player
mapToPlayer playerId left right =
    case playerId of
        App.Left ->
            left

        App.Right ->
            right


init : Session -> App.GameConfig -> ( Model, Cmd Msg )
init s gameConfig =
    let
        left =
            Player.create App.Left

        right =
            Player.create App.Right
    in
    ( { session = s
      , state =
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
      , isFullscreen = False
      , config = gameConfig
      }
    , Cmd.none
    )


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


handleBallsLeftOnTable : Int -> Int -> State -> State
handleBallsLeftOnTable remainingBalls runTo state =
    let
        -- TODO special handling for break fouls
        shotBalls =
            state.ballsOnTable - remainingBalls

        -- TODO replace 'gameFinished' flag by an ADT properly modeling the Game state (Break, Running, Finished)
        gameFinished =
            (state.shooting.points + shotBalls) >= runTo

        playerSwitch =
            if state.switchReason == Foul then
                Yes Foul

            else if gameFinished || (remainingBalls > 1) || (state.ballsOnTable == remainingBalls) then
                Yes state.switchReason

            else
                No

        ( updatedLeft, leftTripleFoul ) =
            -- TODO can't we simply update just the shooting player? . . . Resetting his streak etc. after computing the other values?
            Player.update state.left state.shooting shotBalls playerSwitch runTo

        ( updatedRight, rightTripleFoul ) =
            Player.update state.right state.shooting shotBalls playerSwitch runTo

        ballsToContinueWith =
            if (remainingBalls == 1) || leftTripleFoul || rightTripleFoul then
                fullRack

            else
                remainingBalls

        shootingNext =
            determineShootingNext
                state.shooting.id
                playerSwitch
                updatedLeft
                updatedRight
    in
    { state
        | left = updatedLeft
        , right = updatedRight
        , ballsOnTable = ballsToContinueWith
        , shooting = shootingNext
        , switchReason = Miss
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IsFullscreen bool ->
            ( { model | isFullscreen = bool }, Cmd.none )

        RequestFullscreen bool ->
            ( model, Ports.requestFullscreen bool )

        ToggleFoul ->
            ( handleFoulToggle model, Cmd.none )

        BallsLeftOnTable remainingBalls ->
            let
                newState =
                    handleBallsLeftOnTable remainingBalls model.runTo model.state

                winner =
                    determineWinner model.runTo newState.left newState.right

                modal =
                    case winner of
                        Nothing ->
                            None

                        Just player ->
                            WinnerModal player.id

                newHistory =
                    model.state :: model.history
            in
            ( { model
                | state = newState
                , history = newHistory
                , winner = winner
                , modal = modal
              }
            , Cmd.none
            )

        -- TODO implement me
        ShowLog ->
            ( { model | modal = LogModal }, Cmd.none )

        -- TODO implement me
        CloseLog ->
            ( { model | modal = None }, Cmd.none )

        ExitGame ->
            let
                baseUrl =
                    model.session |> Session.baseUrl

                entrance =
                    baseUrl.path ++ Route.toString Route.Entrance
            in
            ( { model | modal = None }
            , Nav.replaceUrl (model.session |> Session.navKey) entrance
            )


viewBall : (String -> String) -> Int -> Int -> Html Msg
viewBall adaptPath max n =
    let
        visibility =
            if n > max then
                " is-invisible"

            else
                ""

        path =
            adaptPath "img/" ++ String.fromInt n ++ "B.svg"
    in
    button [ class <| "button tile" ++ visibility ]
        [ img
            [ alt (String.fromInt n)
            , src path
            , onClick (BallsLeftOnTable n)
            ]
            []
        ]


viewGameState : State -> Html Msg
viewGameState state =
    let
        isLeftShooting =
            if state.shooting == state.left then
                "has-background-primary"

            else
                ""

        isRightShooting =
            if state.shooting == state.right then
                "has-background-primary"

            else
                ""
    in
    div [ class "columns" ]
        [ div [ class "column has-text-centered", class isLeftShooting ]
            [ text <| String.fromInt state.left.points ]
        , div [ class "column has-text-centered", class isRightShooting ]
            [ text <| String.fromInt state.right.points ]
        ]


viewGameHistory : List State -> Html Msg
viewGameHistory history =
    div []
        (List.map viewGameState history)


viewLogModalDialog : List State -> Html Msg
viewLogModalDialog history =
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
                    [ viewGameHistory history
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


viewModalDialog : Modal -> List State -> Html Msg
viewModalDialog modal history =
    case modal of
        WinnerModal winnerId ->
            viewWinnerModalDialog winnerId

        LogModal ->
            viewLogModalDialog history

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

        fullscreenText =
            if model.isFullscreen then
                "Window"

            else
                "Fullscreen"

        fullscreenMsg =
            RequestFullscreen (not model.isFullscreen)

        baseUrl =
            model.session |> Session.baseUrl
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
                    [ button [ class "button tile", onClick fullscreenMsg, attribute "onClick" "window.enterFullScreen()" ] [ text fullscreenText ]
                    , button [ class "button tile", disabled True ] [ text "RunTo" ]
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
            (List.range 1 fullRack
                |> List.map (viewBall (resourceBasedPath baseUrl) max)
            )
        , viewModalDialog model.modal model.history
        ]


resourceBasedPath : Url -> String -> String
resourceBasedPath baseUrl path =
    baseUrl.path ++ "/../" ++ path
