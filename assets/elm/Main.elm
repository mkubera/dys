module Main exposing (..)

import Html
import Platform.Cmd
import Json.Encode as JE
import Json.Decode as JD exposing (field)
import Dict
import Set
import Model exposing (..)
import View exposing (..)
import Sub exposing (..)


-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- UPDATE


createOutposts : Int -> List Outpost -> List Outpost
createOutposts n list =
    case list of
        [] ->
            list

        first :: rest ->
            { first | id = n } :: (createOutposts (n - 1) rest)


mapTimesN n list =
    case list of
        [] ->
            list

        first :: rest ->
            (first * n) :: (mapTimesN n rest)


filterChosenDiceOutOfRolledDice : List Die -> List Die -> List Die
filterChosenDiceOutOfRolledDice rolledDice uniqueChosenDice =
    List.filter
        (\rolleddie ->
            -- if a chosenDie is also in a newDie, filter it out
            List.all
                (\chosendie -> rolleddie.id /= chosendie.id)
                uniqueChosenDice
        )
        rolledDice


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOfOutposts int ->
            let
                oldSettings =
                    model.settings

                newSettings =
                    { oldSettings | noOfOutposts = int }

                newModel =
                    { model | settings = newSettings }
            in
                newModel ! []

        StartGame ->
            let
                noOfOutposts =
                    model.settings.noOfOutposts

                newOutposts =
                    createOutposts noOfOutposts (List.repeat noOfOutposts (Outpost 0 []))

                oldState =
                    model.state

                newState =
                    { oldState | gameStarted = True }

                newModel =
                    { model | state = newState, outposts = newOutposts }
            in
                newModel ! []

        ToggleDie die ->
            let
                newRolledDice =
                    makeDieChosen die model.rolledDice

                newModel =
                    { model | rolledDice = newRolledDice }
            in
                newModel ! []

        MoveDiceToOutpost outpostId ->
            let
                chosenDice =
                    List.filter (\d -> d.chosen == True) model.rolledDice

                newOutposts =
                    List.map
                        (\o ->
                            if o.id == outpostId then
                                { o | dice = chosenDice ++ o.dice }
                            else
                                o
                        )
                        model.outposts

                newRolledDice =
                    filterChosenDiceOutOfRolledDice model.rolledDice chosenDice
            in
                { model | outposts = newOutposts, rolledDice = newRolledDice } ! []

        SaveDice chosenDice ->
            let
                toggledChosenDice =
                    List.map (\d -> { d | chosen = False }) chosenDice

                newSavedDice =
                    (++) chosenDice model.savedDice

                newRolledDice =
                    filterChosenDiceOutOfRolledDice model.rolledDice chosenDice

                newModel =
                    { model | savedDice = newSavedDice, rolledDice = newRolledDice }
            in
                newModel ! []

        EndTurn ->
            let
                newRolledDice =
                    tempRolledDice model.turn

                newRolledDiceCount =
                    List.length newRolledDice
            in
                { model
                    | turn = model.turn + 2
                    , unitsProduced = model.unitsProduced + newRolledDiceCount
                    , rolledDice = newRolledDice
                }
                    ! []

        NoOp ->
            model ! []


matureSavedDice : List Die -> List Die
matureSavedDice savedDice =
    savedDice
        |> List.map (\d -> { d | age = d.age + 1 })
        |> List.map
            (\d ->
                case d.kind of
                    Fighter ->
                        case d.age of
                            1 ->
                                { d | atk = 6 }

                            2 ->
                                { d | atk = 7, hp = 22 }

                            3 ->
                                { d | atk = 8 }

                            4 ->
                                { d | atk = 9, hp = 24 }

                            5 ->
                                { d | atk = 10, hp = 28 }

                            _ ->
                                { d | atk = 10, hp = 28 }

                    Scout ->
                        case d.age of
                            1 ->
                                { d | hp = 11 }

                            2 ->
                                { d | hp = 12 }

                            3 ->
                                { d | hp = 13 }

                            4 ->
                                { d | hp = 14 }

                            5 ->
                                { d | hp = 15 }

                            _ ->
                                { d | hp = 15 }

                    Grower ->
                        case d.age of
                            1 ->
                                { d | hp = 9, nomi = 3 }

                            2 ->
                                { d | hp = 10, nomi = 4 }

                            3 ->
                                { d | hp = 11, nomi = 5 }

                            4 ->
                                { d | hp = 12, nomi = 6 }

                            5 ->
                                { d | hp = 13, nomi = 7 }

                            _ ->
                                { d | hp = 13, nomi = 7 }

                    Dreamer ->
                        case d.age of
                            1 ->
                                { d | nomi = 10 }

                            2 ->
                                { d | nomi = 12, hp = 7 }

                            3 ->
                                { d | nomi = 14 }

                            4 ->
                                { d | nomi = 16, hp = 8 }

                            5 ->
                                { d | nomi = 20, hp = 10 }

                            _ ->
                                { d | nomi = 20, hp = 10 }
            )


tempRolledDice : Int -> List Die
tempRolledDice turn =
    if List.member turn [ 1, 5 ] then
        [ (Die 2 Scout 1 1 5 1 0 False)
        , (Die 3 Grower 1 1 5 2 0 False)
        , (Die 4 Dreamer 1 1 3 5 0 False)
        ]
    else if List.member turn [ 3, 7 ] then
        [ (Die 1 Fighter 1 3 10 1 0 False)
        , (Die 3 Grower 1 1 5 2 0 False)
        ]
    else
        [ (Die 4 Dreamer 1 1 3 5 0 False) ]
