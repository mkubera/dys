module View exposing (..)

-- import Html exposing (Html, h3, h4, div, text, ul, li, input, form, button, br, hr, table, tbody, tr, td)
-- import Html.Attributes exposing (type_, value)
-- import Html.Events exposing (onInput, onSubmit, onClick)
--

import Html exposing (Html)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Style exposing (StyleSheet)
import Style.Border as Border
import Style.Color as Color
import Color exposing (rgba)


--

import Model exposing (Model, Msg, Msg(..), Location(..))


type Style
    = None
    | ErrMsg
    | Outposts
    | Outpost
    | SavedDice
    | EndTurnMana
    | RolledDice
    | Die
    | ExhaustedDie


stylesheet : StyleSheet Style variation
stylesheet =
    Style.styleSheet
        [ Style.style None []
        , Style.style ErrMsg
            [ Color.background (rgba 75 195 131 1.0)
            ]
        , Style.style Outposts
            [ Border.all 0.1
            , Color.border (rgba 0 0 0 1.0)
            ]
        , Style.style Outpost
            [ Border.all 0.1
            , Color.border (rgba 0 0 0 1.0)
            ]
        , Style.style SavedDice
            [ Border.all 0.1
            , Color.border (rgba 0 0 0 1.0)
            ]
        , Style.style EndTurnMana
            [ Border.all 0.1
            , Color.border (rgba 0 0 0 1.0)
            ]
        , Style.style RolledDice
            [ Border.all 0.1
            , Color.border (rgba 0 0 0 1.0)
            ]
        , Style.style Die
            [ Color.background (rgba 132 185 169 1.0)
            ]
        , Style.style ExhaustedDie
            [ Color.background (rgba 132 145 139 1.0)
            ]
        ]


view : Model -> Html Msg
view model =
    let
        diceToSave =
            List.filter (\d -> d.chosen == True) model.rolledDice
    in
        layout stylesheet <|
            column None
                [ padding 10, spacing 10 ]
                [ viewErrMsg model.errMsg
                , row None [] [ el None [] (text ("TURN: " ++ toString model.turn)) ]
                , column Outposts
                    [ padding 10, spacing 10 ]
                    [ el None [] (text "LANDS")
                    , wrappedRow None [ spacing 10 ] (List.map viewLand model.outposts)
                    ]
                , row None
                    [ spacing 10 ]
                    -- [ column SavedDice
                    --     [ onClick (MoveDice LocationSaved), width fill, padding 10, spacing 10 ]
                    --     [ el None [] (text ("TRAINING"))
                    --     , column None [] (List.map viewSavedDie model.savedDice)
                    --     ]
                    [ column EndTurnMana
                        [ width fill, padding 10, spacing 10 ]
                        [ el None [] (text (""))
                        , column None
                            []
                            [ row None
                                []
                                [ button None [ padding 5, onClick EndTurn ] (text "End Turn")
                                ]
                            , row None
                                []
                                [ el None [] (text ("mana: " ++ (toString model.mana) ++ " / " ++ (toString model.maxMana)))
                                ]
                            ]
                        ]
                    , column RolledDice
                        [ width fill, padding 10, spacing 10 ]
                        [ el None [] (text ("DECK"))
                        , column None [] (List.map viewRolledDie model.rolledDice)
                        ]
                    ]
                , viewStartGame model.state.gameStarted
                ]


viewLand outpost =
    let
        attrs =
            case outpost.cardChosen of
                True ->
                    [ padding 5, width (percent 33.33333333333) ]

                False ->
                    [ onClick (MoveDice (LocationOutpost outpost.id)), padding 5, width (percent 33.33333333333) ]
    in
        column Outpost
            attrs
            [ row None
                []
                [ el None [] (text ("#" ++ (toString outpost.id) ++ " " ++ (toString outpost.kind |> String.toUpper)))
                ]
            , wrappedRow None [] (List.map viewLandDie outpost.dice)
            ]


viewLandDie die =
    let
        style_ =
            case die.exhausted of
                True ->
                    ExhaustedDie

                False ->
                    Die
    in
        el style_ [ onClick (ToggleDie die), padding 5 ] (text ((toString die.kind) ++ " #" ++ (toString die.id) ++ " (atk: " ++ (toString die.atk) ++ ", hp: " ++ (toString die.hp) ++ ")"))



-- viewSavedDie die =
--     let
--         style_ =
--             case die.exhausted of
--                 True ->
--                     ExhaustedDie
--
--                 False ->
--                     Die
--     in
--         el style_ [ padding 5 ] (text ((toString die.kind) ++ " #" ++ (toString die.id) ++ " (age: " ++ (toString die.age) ++ ", mana: " ++ (toString die.mana.savedToOutpost) ++ ")"))


viewRolledDie die =
    let
        style_ =
            case die.exhausted of
                True ->
                    ExhaustedDie

                False ->
                    Die
    in
        el style_ [ onClick (ToggleDie die), padding 5 ] (text ((toString die.kind) ++ " #" ++ (toString die.id) ++ " (chosen: " ++ (toString die.chosen) ++ ")"))


viewStartGame gameStarted =
    case gameStarted of
        True ->
            empty

        False ->
            column None
                []
                [ row None
                    [ spacing 5 ]
                    [ button None [ padding 5, onClick (StartGame True) ] (text "Start Game")
                    ]
                , row None [ paddingXY 0 10, spacing 5 ] [ el None [] (text "Defend Your Soul: a shamanic game of love & protection") ]
                ]


viewErrMsg errMsg =
    let
        errMsg_ =
            Maybe.withDefault "" errMsg
    in
        case errMsg of
            Nothing ->
                empty

            Just _ ->
                full ErrMsg [ padding 10 ] (el None [] (text errMsg_))
