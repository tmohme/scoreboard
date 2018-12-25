module Game exposing
    ( Model
    , Msg(..)
    , Player
    , calculateCurrentStreak
    , createPlayer
    , determineShootingNext
    , determineWinner
    , init
    , start
    , update
    , updatedPlayer
    , view
    )

import Application as App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode


type alias Player =
    { id : App.PlayerId
    , points : Int
    , innings : Int
    , currentStreak : Int
    , longestStreak : Int
    , pointsAtStreakStart : Int
    }


type Msg
    = BallsLeftOnTable Int
    | WinnerShown


type alias Model =
    { left : Player
    , right : Player
    , shooting : Maybe Player
    , ballsLeftOnTable : Int
    , runTo : Maybe Int
    , winner : Maybe Player
    , showWinner : ShowWinner
    }


type ShowWinner
    = NotYet
    | ShowWinner App.PlayerId
    | AlreadyShown


fullRack =
    15


init : Model
init =
    { left = createPlayer App.Left
    , right = createPlayer App.Right
    , shooting = Nothing
    , ballsLeftOnTable = fullRack
    , runTo = Nothing
    , winner = Nothing
    , showWinner = NotYet
    }


start : Model -> Maybe App.PlayerId -> Maybe Int -> Model
start model breaker runToPoints =
    { model
        | shooting =
            case breaker of
                Just playerId ->
                    case playerId of
                        App.Left ->
                            Just model.left

                        App.Right ->
                            Just model.right

                Nothing ->
                    Nothing
        , runTo = runToPoints
    }


createPlayer : App.PlayerId -> Player
createPlayer id =
    Player id 0 0 0 0 0


calculateCurrentStreak : Int -> Int -> Int -> Int
calculateCurrentStreak previous increment limit =
    Basics.min (previous + increment) limit


updatedPlayer : Player -> Maybe Player -> Int -> Bool -> Int -> Player
updatedPlayer player shooting shotBalls playerSwitch runToPoints =
    case shooting of
        Just someone ->
            if someone.id == player.id then
                -- TODO extract branch
                let
                    points =
                        Basics.min (player.points + shotBalls) runToPoints

                    inningIncrement =
                        if playerSwitch then
                            1

                        else
                            0

                    maxBallsToRun =
                        runToPoints - player.pointsAtStreakStart

                    currentStreak =
                        calculateCurrentStreak player.currentStreak shotBalls maxBallsToRun

                    longestStreak =
                        Basics.max player.longestStreak currentStreak
                in
                { player
                    | points = points
                    , innings = player.innings + inningIncrement
                    , currentStreak = currentStreak
                    , longestStreak = longestStreak
                }

            else
                { player
                    | currentStreak = 0
                    , pointsAtStreakStart = player.points
                }

        Nothing ->
            player


determineShootingNext : Maybe App.PlayerId -> Bool -> Player -> Player -> Maybe Player
determineShootingNext shootingPrevious playerSwitch left right =
    if playerSwitch then
        case shootingPrevious of
            Just id ->
                if id == left.id then
                    Just right

                else
                    Just left

            Nothing ->
                Nothing

    else
        case shootingPrevious of
            Just id ->
                if id == left.id then
                    Just left

                else
                    Just right

            Nothing ->
                Nothing


determineWinner : Maybe Int -> Player -> Player -> Maybe Player
determineWinner runToPoints left right =
    case runToPoints of
        Just points ->
            if left.points >= points then
                Just left

            else if right.points >= points then
                Just right

            else
                Nothing

        Nothing ->
            Nothing


update : Msg -> Model -> Model
update msg model =
    case msg of
        BallsLeftOnTable n ->
            let
                shotBalls =
                    model.ballsLeftOnTable - n

                gameFinished =
                    case ( model.shooting, model.runTo ) of
                        ( Just player, Just runToPoints ) ->
                            (player.points + shotBalls) >= runToPoints

                        ( _, _ ) ->
                            False

                playerSwitch =
                    gameFinished || (n > 1) || (model.ballsLeftOnTable == n)

                left =
                    updatedPlayer model.left model.shooting shotBalls playerSwitch (model.runTo |> Maybe.withDefault 0)

                right =
                    updatedPlayer model.right model.shooting shotBalls playerSwitch (model.runTo |> Maybe.withDefault 0)

                ballsOnTable =
                    if n == 1 then
                        fullRack

                    else
                        n

                shootingNext =
                    determineShootingNext
                        (model.shooting
                            |> Maybe.map .id
                        )
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
            if player.innings > 0 then
                (round ((toFloat player.points / toFloat player.innings) * 10.0) |> toFloat) / 10.0

            else
                0.0
    in
    div []
        [ p [ class ("big-auto-size" ++ " " ++ style) ] [ text (player.points |> String.fromInt) ]
        , div [ class ("level" ++ " " ++ style) ]
            [ div [ class "level-item has-text-centered" ]
                [ p [ class "heading" ] [ text "AN" ]
                , p [ class "title" ] [ text (player.innings |> String.fromInt) ]
                ]
            , div [ class "level-item has-text-centered" ]
                [ p [ class "heading" ] [ text "GD" ]
                , p [ class "title" ] [ text (totalAvg |> String.fromFloat) ]
                ]
            , div [ class "level-item has-text-centered" ]
                [ p [ class "heading" ] [ text "HA" ]
                , p [ class "title" ] [ text (player.longestStreak |> String.fromInt) ]
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
            if n > max then
                Html.Attributes.hidden True

            else
                Html.Attributes.hidden False
    in
    button [ maybeHidden, class "button tile" ]
        [ img [ alt (String.fromInt n), src ("img/" ++ String.fromInt n ++ "B.svg"), onClick (BallsLeftOnTable n) ] []
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
            model.shooting == Just model.left

        isRightShooting =
            model.shooting == Just model.right

        max =
            model.ballsLeftOnTable
    in
    div
        []
        [ div
            [ class "columns" ]
            [ div [ class "column is-two-fifth is-centered has-text-centered" ] [ viewPlayer model.left isLeftShooting ]
            , div [ class "column is-one-fifth is-centered tile is-ancestor is-vertical" ]
                [ button [ class "button tile", disabled True ] [ text "Vollbild" ]
                , button [ class "button tile", disabled True ] [ text "RunTo" ]
                , button [ class "button tile", disabled True ] [ text "Pause / Weiter" ]
                , button [ class "button tile", disabled True ] [ text "Log / Undo" ]
                , button [ class "button tile", disabled True ] [ text "Ende" ]
                ]
            , div [ class "column is-two-fifth is-centered has-text-centered" ] [ viewPlayer model.right isRightShooting ]
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
