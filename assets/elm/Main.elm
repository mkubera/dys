module Main exposing (..)

import Html
import Platform.Cmd
import Model exposing (..)
import View exposing (view)
import Sub exposing (..)
import Update.Helpers exposing (..)


-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame bool ->
            let
                oldState =
                    model.state

                newState =
                    { oldState | gameStarted = True }

                oldFighter =
                    base.fighter

                oldScout =
                    base.scout

                oldChangeLand =
                    base.changeLand

                newFighter =
                    CardUnit { oldFighter | id = 1 }

                newScout =
                    CardUnit { oldScout | id = 2 }

                newChangeLandSpell =
                    CardSpell { oldChangeLand | id = 3 }

                newDeck =
                    [ newFighter, newScout, newChangeLandSpell ]

                newUnitsProduced =
                    List.length newDeck

                newModel =
                    { model
                        | state = newState
                        , deck = newDeck
                        , unitsProduced = newUnitsProduced
                        , mana = 2
                        , maxMana = 2
                    }
            in
                newModel ! []

        ToggleCard card ->
            let
                newChosenCard =
                    case model.chosenCard == Just card of
                        True ->
                            Nothing

                        False ->
                            Just card

                newModel =
                    { model | chosenCard = newChosenCard }
            in
                newModel ! []

        TogglePreview previewed ->
            let
                newPreviewed =
                    case model.previewed of
                        Just _ ->
                            Nothing

                        Nothing ->
                            Just previewed
            in
                { model | previewed = newPreviewed } ! []

        UseCard maybeLandId ->
            if (model.chosenCard == Nothing) then
                model ! []
            else if (isExhausted model.chosenCard) then
                model ! []
            else if (isDeployed model.chosenCard) && (landIsntAdjacent model.chosenCard maybeLandId) then
                model ! []
            else if (notEnoughMana model.chosenCard model.mana) then
                { model | errMsg = Just "Not enough mana." } ! []
            else
                let
                    chosenCard =
                        Maybe.withDefault CardNothing model.chosenCard
                in
                    case maybeLandId of
                        Nothing ->
                            -- global Spell
                            model ! []

                        Just landId ->
                            case chosenCard of
                                CardUnit _ ->
                                    -- Unit
                                    (model
                                        |> payCardCost
                                        |> removeChosenCardFromDeck
                                        |> deployOrMoveChosenUnitToLand landId
                                        |> updateChosenCard Nothing
                                    )
                                        ! []

                                CardSpell chosenSpell ->
                                    -- land Spell
                                    case chosenSpell.kind of
                                        ChangeLand ->
                                            case landId == initLandHeartId of
                                                True ->
                                                    { model | errMsg = Just "Can't change the Heart." } ! []

                                                False ->
                                                    let
                                                        oldModals =
                                                            model.modals

                                                        newModals =
                                                            { oldModals | landChanged = True }
                                                    in
                                                        { model
                                                            | modals = newModals
                                                            , chosenLandId = Just landId
                                                        }
                                                            ! []

                                CardNothing ->
                                    model ! []

        UnitAttackEnemy enemyId ->
            (model
                |> exhaustChosenUnit
                |> applyDamageToEnemy enemyId
                |> removeDeadEnemies
                |> updateChosenCard Nothing
            )
                ! []

        ChangeChosenLand kind ->
            let
                chosenLandId =
                    Maybe.withDefault 0 model.chosenLandId

                newLands =
                    model.lands
                        |> List.map
                            (\l ->
                                case l.id == chosenLandId of
                                    True ->
                                        { l | kind = kind }

                                    False ->
                                        l
                            )
            in
                ({ model
                    | lands = newLands
                    , modals = initModals
                 }
                    |> payCardCost
                    |> removeChosenCardFromDeck
                    |> updateChosenCard Nothing
                    |> updateChosenLandId Nothing
                )
                    ! []

        EndTurn ->
            let
                newLands =
                    model.lands
                        |> List.map
                            (\l ->
                                let
                                    oldLandUnits =
                                        l.units

                                    newLandUnits =
                                        oldLandUnits
                                            |> List.map
                                                (\c ->
                                                    case l.kind == Heart of
                                                        True ->
                                                            c
                                                                |> matureUnit
                                                                |> unexhaustUnit
                                                                |> ageUnit

                                                        False ->
                                                            c
                                                                |> unexhaustUnit
                                                                |> ageUnit
                                                )
                                in
                                    { l | units = newLandUnits }
                            )

                newDeck =
                    case List.length model.deck >= 5 of
                        True ->
                            model.deck

                        False ->
                            (++) model.deck (tempRandomDeckCards ( model.turn, model.unitsProduced ))

                newDeckCount =
                    List.length newDeck

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
                    , unitsProduced = model.unitsProduced + 1
                    , lands = newLands
                    , deck = newDeck
                    , mana = newMana
                    , maxMana = newMaxMana
                    , errMsg = Nothing
                }
                    ! []

        NoOp ->
            model ! []
