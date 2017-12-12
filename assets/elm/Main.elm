module Main exposing (..)

import Html
import Platform.Cmd
import Json.Encode as JE
import Json.Decode as JD exposing (field)
import Dict
import Set
import Model exposing (..)
import View exposing (view)
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


makeDieChosen : Die -> List Die -> List Die
makeDieChosen die dice =
    List.map
        (\d ->
            case d == die of
                True ->
                    { die | chosen = not die.chosen }

                False ->
                    d
        )
        dice



-- createOutposts : Int -> List Outpost -> List Outpost
-- createOutposts n list =
--     case list of
--         [] ->
--             list
--
--         first :: [] ->
--             let
--                 baseShaman =
--                     base.shaman
--
--                 newShaman =
--                     { baseShaman | id = 1 }
--             in
--                 { first | id = n, dice = [ newShaman ] } :: (createOutposts (n - 1) [])
--
--         first :: rest ->
--             { first | id = n } :: (createOutposts (n - 1) rest)


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


costOfDice : List Die -> Location Int -> Int
costOfDice dice targetLocation =
    dice
        |> List.map
            (\d ->
                case d.location of
                    LocationRolled ->
                        case targetLocation of
                            LocationRolled ->
                                0

                            LocationSaved ->
                                d.mana.rolledToSaved

                            LocationOutpost a ->
                                d.mana.rolledToOutpost

                    LocationSaved ->
                        d.mana.rolledToOutpost

                    LocationOutpost a ->
                        0
            )
        |> List.foldl (+) 0


hasEnoughMana : Int -> List Die -> Location Int -> Bool
hasEnoughMana mana dice targetLocation =
    mana >= (costOfDice dice targetLocation)


diceWithinLimit : List Die -> Bool
diceWithinLimit dice =
    List.length dice <= 3


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

        StartGame bool ->
            let
                noOfOutposts =
                    model.settings.noOfOutposts

                baseShaman =
                    base.shaman

                newShaman =
                    { baseShaman | id = 1 }

                newOutposts =
                    initOutposts

                -- createOutposts noOfOutposts (List.repeat noOfOutposts (Outpost 0 [])) |> List.reverse
                oldState =
                    model.state

                newState =
                    { oldState | gameStarted = True }

                oldFighter =
                    base.fighter

                oldScout =
                    base.scout

                newFighter =
                    { oldFighter | id = 2 }

                newScout =
                    { oldScout | id = 3 }

                newRolledDice =
                    [ newFighter, newScout ]

                newModel =
                    { model | state = newState, outposts = newOutposts, rolledDice = newRolledDice }
            in
                newModel ! []

        ToggleDie die ->
            let
                newRolledDice =
                    makeDieChosen die model.rolledDice

                newLands =
                    model.outposts
                        |> List.map (\l -> { l | cardChosen = (not l.cardChosen) })

                newModel =
                    { model | rolledDice = newRolledDice, outposts = newLands, chosenCard = Just die }
            in
                newModel ! []

        MoveDice targetLocation ->
            let
                chosenDice =
                    model.rolledDice
                        -- choose chosen dice
                        |>
                            List.filter (\d -> d.chosen == True)
                        -- reset chosen dice
                        |>
                            List.map (\d -> { d | chosen = False, exhausted = True })

                l1 =
                    Debug.log "targetLocation" targetLocation

                l2 =
                    Debug.log "chosenDice" chosenDice

                hasEnoughMana_ =
                    hasEnoughMana model.mana chosenDice targetLocation

                targetOutpostId =
                    case targetLocation of
                        LocationRolled ->
                            0

                        LocationSaved ->
                            0

                        LocationOutpost outpostId ->
                            outpostId

                newOutposts =
                    case hasEnoughMana_ && (targetOutpostId /= 0) of
                        True ->
                            List.map
                                (\o ->
                                    if o.id == targetOutpostId then
                                        let
                                            newChosenDice =
                                                chosenDice
                                                    |> List.map (\d -> { d | location = LocationOutpost targetOutpostId })
                                        in
                                            { o | dice = newChosenDice ++ o.dice }
                                    else
                                        o
                                )
                                model.outposts

                        False ->
                            model.outposts

                newSavedDice =
                    case hasEnoughMana_ && (targetLocation == LocationSaved) && (diceWithinLimit model.savedDice) of
                        True ->
                            let
                                newChosenDice =
                                    chosenDice
                                        |> List.map (\d -> { d | location = LocationSaved })
                            in
                                (++) newChosenDice model.savedDice

                        False ->
                            model.savedDice

                newRolledDice =
                    case hasEnoughMana_ of
                        True ->
                            filterChosenDiceOutOfRolledDice model.rolledDice chosenDice

                        False ->
                            model.rolledDice

                newMana =
                    case model.mana >= 1 && hasEnoughMana_ of
                        True ->
                            model.mana - (costOfDice chosenDice targetLocation)

                        False ->
                            model.mana

                errMsg =
                    case hasEnoughMana_ of
                        True ->
                            Nothing

                        False ->
                            Just "Not enough mana."
            in
                { model
                    | outposts = newOutposts
                    , savedDice = newSavedDice
                    , rolledDice = newRolledDice
                    , mana = newMana
                    , errMsg = errMsg
                }
                    ! []

        EndTurn ->
            let
                newLandDice =
                    model.outposts
                        |> List.map
                            (\land ->
                                let
                                    oldDice =
                                        land.dice

                                    newDice =
                                        oldDice
                                            |> List.map
                                                (\d ->
                                                    { d
                                                        | exhausted = False
                                                        , chosen = False
                                                    }
                                                )
                                            |> List.map
                                                (\d ->
                                                    case land.kind == Heart of
                                                        True ->
                                                            matureDie d

                                                        False ->
                                                            d
                                                )
                                in
                                    { land | dice = newDice }
                            )

                -- heartDice =
                --     model.outposts
                --         |> List.filter (\o -> o.kind == Heart)
                --
                -- newSavedDice =
                --     matureDice heartDice
                newRolledDice =
                    case List.length model.rolledDice >= 5 of
                        True ->
                            model.rolledDice

                        False ->
                            (++) model.rolledDice (tempRolledDice ( model.turn, model.unitsProduced ))

                newRolledDiceCount =
                    List.length newRolledDice

                newMaxMana =
                    case model.maxMana >= 5 of
                        True ->
                            model.maxMana

                        False ->
                            model.maxMana + 1

                newMana =
                    newMaxMana
            in
                { model
                    | turn = model.turn + 2
                    , unitsProduced = model.unitsProduced + newRolledDiceCount
                    , outposts =
                        newLandDice
                        -- , savedDice = newSavedDice
                    , rolledDice = newRolledDice
                    , mana = newMana
                    , maxMana = newMaxMana
                    , errMsg = Nothing
                }
                    ! []

        NoOp ->
            model ! []


matureDie : Die -> Die
matureDie die =
    case die.kind of
        Fighter ->
            let
                oldMana =
                    die.mana
            in
                case die.age of
                    1 ->
                        { die | atk = 6, hp = 20, mana = { oldMana | savedToOutpost = 1 } }

                    3 ->
                        { die | atk = 7, hp = 22, mana = { oldMana | savedToOutpost = 2 } }

                    5 ->
                        { die | atk = 8, hp = 25, mana = { oldMana | savedToOutpost = 3 } }

                    _ ->
                        { die | atk = 8, hp = 25, mana = { oldMana | savedToOutpost = 3 } }

        Scout ->
            let
                oldMana =
                    die.mana
            in
                case die.age of
                    1 ->
                        { die | hp = 11, mana = { oldMana | savedToOutpost = 1 } }

                    3 ->
                        { die | hp = 12, mana = { oldMana | savedToOutpost = 2 } }

                    5 ->
                        { die | hp = 15, mana = { oldMana | savedToOutpost = 3 } }

                    _ ->
                        { die | hp = 15, mana = { oldMana | savedToOutpost = 3 } }

        Shaman ->
            let
                oldMana =
                    die.mana
            in
                case die.age of
                    1 ->
                        { die | nomi = 3, mana = { oldMana | savedToOutpost = 1 } }

                    3 ->
                        { die | nomi = 4, mana = { oldMana | savedToOutpost = 2 } }

                    5 ->
                        { die | nomi = 7, mana = { oldMana | savedToOutpost = 3 } }

                    _ ->
                        { die | nomi = 7, mana = { oldMana | savedToOutpost = 3 } }


matureDice : List Die -> List Die
matureDice dice =
    dice
        |> List.map
            (\d ->
                case d.age >= 5 of
                    True ->
                        d

                    False ->
                        { d | age = d.age + 1 }
            )
        |> List.map
            (\d ->
                case d.kind of
                    Fighter ->
                        let
                            oldMana =
                                d.mana
                        in
                            case d.age of
                                1 ->
                                    { d | atk = 6, hp = 20, mana = { oldMana | savedToOutpost = 1 } }

                                3 ->
                                    { d | atk = 7, hp = 22, mana = { oldMana | savedToOutpost = 2 } }

                                5 ->
                                    { d | atk = 8, hp = 25, mana = { oldMana | savedToOutpost = 3 } }

                                _ ->
                                    { d | atk = 8, hp = 25, mana = { oldMana | savedToOutpost = 3 } }

                    Scout ->
                        let
                            oldMana =
                                d.mana
                        in
                            case d.age of
                                1 ->
                                    { d | hp = 11, mana = { oldMana | savedToOutpost = 1 } }

                                3 ->
                                    { d | hp = 12, mana = { oldMana | savedToOutpost = 2 } }

                                5 ->
                                    { d | hp = 15, mana = { oldMana | savedToOutpost = 3 } }

                                _ ->
                                    { d | hp = 15, mana = { oldMana | savedToOutpost = 3 } }

                    Shaman ->
                        let
                            oldMana =
                                d.mana
                        in
                            case d.age of
                                1 ->
                                    { d | nomi = 3, mana = { oldMana | savedToOutpost = 1 } }

                                3 ->
                                    { d | nomi = 4, mana = { oldMana | savedToOutpost = 2 } }

                                5 ->
                                    { d | nomi = 7, mana = { oldMana | savedToOutpost = 3 } }

                                _ ->
                                    { d | nomi = 7, mana = { oldMana | savedToOutpost = 3 } }
            )


tempRolledDice : ( Int, Int ) -> List Die
tempRolledDice ( turn, unitsProduced ) =
    let
        newFighter =
            base.fighter

        newScout =
            base.scout
    in
        if List.member turn [ 1, 5, 9, 13 ] then
            [ { newScout | id = unitsProduced + 1 }
            ]
        else
            [ { newFighter | id = unitsProduced + 1 } ]
