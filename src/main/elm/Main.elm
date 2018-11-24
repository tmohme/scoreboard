module Main exposing (Model, Msg(..), Page(..), Player, PlayerId(..), ShowWinner(..), breakButton, bulma, calculateCurrentStreak, canBreak, createPlayer, css, determineShootingNext, determineWinner, init, main, runToHtml, subscriptions, update, updatedPlayer, view, viewBall, viewBody, viewEntrance, viewGame, viewHeader, viewPlayer, viewRunToModalDialog, viewWinnerModalDialog)

-- import Debug exposing (log)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (..)


type Page
    = Entrance
    | Game


type ShowWinner
    = NotYet
    | ShowWinner PlayerId
    | AlreadyShown


type Msg
    = LeftBreak
    | RightBreak
    | RunToInput String
    | SetRunTo
    | ToggleRunTo
    | BallsLeftOnTable Int
    | WinnerShown


type alias Model =
    { runTo : Maybe Int
    , runToBuffer : Maybe Int
    , isSettingRunTo : Bool
    , page : Page
    , left : Player
    , right : Player
    , shooting : Maybe Player
    , ballsLeftOnTable : Int
    , winner : Maybe Player
    , showWinner : ShowWinner
    }


type PlayerId
    = Left
    | Right


toString : PlayerId -> String
toString id =
    case id of
        Left ->
            "left"

        Right ->
            "right"


type alias Player =
    { id : PlayerId
    , points : Int
    , innings : Int
    , currentStreak : Int
    , longestStreak : Int
    , pointsAtStreakStart : Int
    }


createPlayer : PlayerId -> Player
createPlayer id =
    Player id 0 0 0 0 0


init : () -> ( Model, Cmd Msg )
init _ =
    ( { runTo = Nothing
      , runToBuffer = Just 80
      , isSettingRunTo = False
      , page = Entrance
      , left = createPlayer Left
      , right = createPlayer Right
      , shooting = Nothing
      , ballsLeftOnTable = 15
      , winner = Nothing
      , showWinner = NotYet
      }
    , Cmd.none
    )


canBreak : Model -> Bool
canBreak model =
    isJust model.runTo


calculateCurrentStreak : Int -> Int -> Int -> Int
calculateCurrentStreak previous increment limit =
    Basics.min (previous + increment) limit


updatedPlayer : Player -> Maybe Player -> Int -> Bool -> Int -> Player
updatedPlayer player shooting shotBalls playerSwitch runTo =
    case shooting of
        Just someone ->
            if someone.id == player.id then
                -- TODO extract branch
                let
                    points =
                        Basics.min (player.points + shotBalls) runTo

                    inningIncrement =
                        if playerSwitch then
                            1

                        else
                            0

                    maxBallsToRun =
                        runTo - player.pointsAtStreakStart

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


determineShootingNext : Maybe PlayerId -> Bool -> Player -> Player -> Maybe Player
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
determineWinner runTo left right =
    case runTo of
        Just value ->
            if left.points >= value then
                Just left

            else if right.points >= value then
                Just right

            else
                Nothing

        Nothing ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LeftBreak ->
            ( { model | page = Game, shooting = Just model.left }, Cmd.none )

        RightBreak ->
            ( { model | page = Game, shooting = Just model.right }, Cmd.none )

        RunToInput s ->
            ( { model | runToBuffer = String.toInt s }, Cmd.none )

        SetRunTo ->
            ( { model | isSettingRunTo = False, runTo = model.runToBuffer }
            , Cmd.none
            )

        ToggleRunTo ->
            ( { model | isSettingRunTo = not model.isSettingRunTo }
            , Cmd.none
            )

        BallsLeftOnTable n ->
            let
                shotBalls =
                    model.ballsLeftOnTable - n

                gameFinished =
                    case ( model.shooting, model.runTo ) of
                        ( Just player, Just runTo ) ->
                            (player.points + shotBalls) >= runTo

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
                        15

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
            ( { model
                | ballsLeftOnTable = ballsOnTable
                , left = left
                , right = right
                , shooting = shootingNext
                , winner = winner
                , showWinner = showWinner
              }
            , Cmd.none
            )

        WinnerShown ->
            ( { model | showWinner = AlreadyShown }
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


runToHtml : Html Msg
runToHtml =
    button [ onClick ToggleRunTo, class "button" ]
        [ text "run to ..." ]


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


viewWinnerModalDialog : PlayerId -> Html Msg
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
                        , text (toString playerId)
                        ]
                    ]
                , footer [ class "modal-card-foot" ]
                    [ button [ type_ "button", class "button is-primary", onClick WinnerShown, attribute "aria-label" "OK" ]
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
            [ runToHtml ]
        , div [ class "level-item has-test-cenered" ]
            [ breakButton model RightBreak ]
        , if model.isSettingRunTo then
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
    div [ maybeHidden ]
        [ img [ alt (String.fromInt n), src ("img/" ++ String.fromInt n ++ "B.svg"), onClick (BallsLeftOnTable n) ] []
        ]


viewGame : Model -> Html Msg
viewGame model =
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
        , case model.showWinner of
            ShowWinner winnerId ->
                viewWinnerModalDialog winnerId

            _ ->
                text ""
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



--


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
