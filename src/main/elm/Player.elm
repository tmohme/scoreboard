module Player exposing (Player, PlayerSwitch(..), calculateCurrentStreak, create, update, view)

import Application as App
import Html exposing (..)
import Html.Attributes exposing (..)


type PlayerSwitch
    = No
    | Yes Bool -- indicate with foul or not


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


update : Player -> Player -> Int -> PlayerSwitch -> Int -> Player
update player shooting shotBalls playerSwitch runToPoints =
    -- TODO Introduce concept of "shootingPlayer" (Player+Bool)!?
    if shooting.id == player.id then
        -- TODO extract branch
        let
            points =
                Basics.min (player.points + shotBalls) runToPoints

            inningIncrement =
                case playerSwitch of
                    Yes _ ->
                        1

                    No ->
                        0

            fouls =
                case playerSwitch of
                    Yes True ->
                        if player.previousFouls == 2 then
                            0

                        else
                            player.previousFouls + 1

                    _ ->
                        0

            foulMalus =
                case playerSwitch of
                    Yes True ->
                        if (player.previousFouls == 2) then
                            (1 + 15)
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
        { player
            | points = points - foulMalus
            , innings = player.innings + inningIncrement
            , previousFouls = fouls
            , currentStreak = currentStreak
            , longestStreak = longestStreak
        }

    else
        { player
            | currentStreak = 0
            , pointsAtStreakStart = player.points
        }


view : Player -> Bool -> Html msg
view player isShooting =
    let
        style =
            case isShooting of
                True ->
                    " has-background-primary"

                False ->
                    ""

        totalAvg =
            if player.innings > 0 then
                (round ((toFloat player.points / toFloat player.innings) * 10.0) |> toFloat) / 10.0

            else
                0.0
    in
    div []
        [ p [ class <| "big-auto-size" ++ style ] [ text (player.points |> String.fromInt) ]
        , div [ class <| "level" ++ style ]
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
