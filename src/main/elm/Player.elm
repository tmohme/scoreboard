module Player exposing (Player, PlayerSwitch(..), SwitchReason(..), calculateCurrentStreak, create, update, view)

import Application as App
import Html exposing (..)
import Html.Attributes exposing (..)


type SwitchReason
    = Miss
    | Foul


type PlayerSwitch
    = No
    | Yes SwitchReason


type alias Player =
    { id : App.PlayerId
    , points : Int
    , innings : Int
    , currentStreak : Int
    , longestStreak : Int
    , pointsAtStreakStart : Int
    , previousFouls : Int
    }


create : App.PlayerId -> Player
create id =
    Player id 0 0 0 0 0 0


calculateCurrentStreak : Int -> Int -> Int -> Int
calculateCurrentStreak previous increment limit =
    Basics.min (previous + increment) limit


update : Player -> Player -> Int -> PlayerSwitch -> Int -> ( Player, Bool )
update player shooting shotBalls playerSwitch runToPoints =
    -- TODO Introduce concept of "shootingPlayer" (Player+Bool)!?
    if shooting.id == player.id then
        -- TODO extract branch
        let
            points =
                Basics.min (player.points + shotBalls) runToPoints

            overshot =
                (player.points + shotBalls) > runToPoints

            inningIncrement =
                case playerSwitch of
                    Yes _ ->
                        1

                    No ->
                        0

            fouls =
                if overshot then
                    player.previousFouls

                else
                    case playerSwitch of
                        Yes Foul ->
                            if player.previousFouls == 2 then
                                0

                            else
                                player.previousFouls + 1

                        _ ->
                            0

            tripleFoulPenalty =
                case playerSwitch of
                    Yes Foul ->
                        player.previousFouls == 2

                    _ ->
                        False

            foulMalus =
                if overshot then
                    0

                else
                    case playerSwitch of
                        Yes Foul ->
                            if tripleFoulPenalty then
                                1 + 15

                            else
                                1

                        _ ->
                            0

            maxBallsToRun =
                runToPoints - player.pointsAtStreakStart

            currentStreak =
                calculateCurrentStreak player.currentStreak shotBalls maxBallsToRun

            longestStreak =
                Basics.max player.longestStreak currentStreak
        in
        ( { player
            | points = points - foulMalus
            , innings = player.innings + inningIncrement
            , previousFouls = fouls
            , currentStreak = currentStreak
            , longestStreak = longestStreak
          }
        , tripleFoulPenalty
        )

    else
        ( { player
            | currentStreak = 0
            , pointsAtStreakStart = player.points
          }
        , False
        )


view : Player -> Bool -> Html msg
view player isShooting =
    let
        shooting =
            if isShooting then
                "has-background-primary"

            else
                ""

        totalAvg =
            if player.innings > 0 then
                (round ((toFloat player.points / toFloat player.innings) * 10.0) |> toFloat) / 10.0

            else
                0.0

        foulWarning =
            if player.previousFouls == 2 then
                "has-text-danger"

            else
                ""
    in
    div []
        [ p [ class "big-auto-size", class shooting ] [ text (player.points |> String.fromInt) ]
        , div [ class "level", class shooting ]
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
                [ p [ class "heading", class foulWarning ] [ text "Fouls" ]
                , p [ class "title", class foulWarning ] [ text (player.previousFouls |> String.fromInt) ]
                ]
            ]
        ]
