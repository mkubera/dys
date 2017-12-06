module View exposing (..)

import Html exposing (Html, h3, h4, div, text, ul, li, input, form, button, br, hr, table, tbody, tr, td)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput, onSubmit, onClick)
import Model exposing (..)


view : Model -> Html Msg
view model =
    let
        diceToSave =
            List.filter (\d -> d.chosen == True) model.rolledDice

        view_ =
            case model.state.gameStarted of
                True ->
                    div []
                        -- [ text (toString (mapTimesN 2 [ 1, 2, 3 ]))
                        [ text ("COUNTER: " ++ (toString model.turn))
                        , br [] []
                        , button [ onClick (SaveDice diceToSave) ] [ text "Save Dice" ]
                        , button [ onClick EndTurn ] [ text "End Turn" ]
                        , br [] []
                        , viewOutposts model.outposts
                        , viewSavedDice model.savedDice
                        , viewRolledDice model.rolledDice
                        ]

                False ->
                    div []
                        [ viewStart
                        ]
    in
        view_


viewOutposts : List Outpost -> Html Msg
viewOutposts outposts =
    div []
        [ h4 [] [ text "OUTPOSTS" ]
        , div [] (List.map viewOutpost outposts)
        ]


viewOutpost : Outpost -> Html Msg
viewOutpost outpost =
    div [ onClick (MoveDiceToOutpost outpost.id) ]
        [ text ("id: " ++ (toString outpost.id))
        , div [] (List.map viewOutpostDie outpost.dice)
        ]


viewOutpostDie : Die -> Html Msg
viewOutpostDie die =
    div []
        [ text (toString die.kind)
        , text (" (id: " ++ (toString die.id) ++ ")")
        ]


viewSavedDice : List Die -> Html Msg
viewSavedDice savedDice =
    div []
        [ h4 [] [ text "SAVED" ]
        , div [] (List.map viewDie savedDice)
        ]


viewRolledDice : List Die -> Html Msg
viewRolledDice rolledDice =
    div []
        [ h4 [] [ text "ROLLED" ]
        , div [] (List.map viewDie rolledDice)
        ]


viewDie : Die -> Html Msg
viewDie die =
    div [ onClick (ToggleDie die) ]
        [ text (toString die.kind)
        , text (" (id: " ++ (toString die.id) ++ ")")
        , text (" (chosen: " ++ (toString die.chosen) ++ ")")
        ]


viewStart : Html Msg
viewStart =
    div []
        [ button [ onClick (NoOfOutposts 1) ] [ text "1" ]
        , button [ onClick (NoOfOutposts 2) ] [ text "2" ]
        , button [ onClick (NoOfOutposts 3) ] [ text "3" ]
        , button [ onClick StartGame ] [ text "Start Game" ]
        ]
