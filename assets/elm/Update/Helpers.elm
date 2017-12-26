module Update.Helpers exposing (..)

import Model exposing (..)


payCardCost : Model -> Model
payCardCost model =
    let
        chosenCard =
            unpackCard model.chosenCard

        newMana =
            case chosenCard of
                CardUnit unit ->
                    -- check if Unit isn't already deployed
                    case unit.landId of
                        Just _ ->
                            model.mana

                        Nothing ->
                            model.mana - unit.mana

                CardSpell spell ->
                    model.mana - spell.mana

                CardNothing ->
                    model.mana
    in
        { model | mana = newMana }


removeChosenCardFromDeck : Model -> Model
removeChosenCardFromDeck model =
    let
        chosenCard =
            unpackCard model.chosenCard

        newDeck =
            model.deck |> List.filter (\c -> c /= chosenCard)
    in
        { model | deck = newDeck }


deployOrMoveChosenUnitToLand : Int -> Model -> Model
deployOrMoveChosenUnitToLand landId model =
    case unpackCard model.chosenCard of
        CardUnit chosenUnit ->
            let
                landUnit =
                    chosenUnit |> exhaustUnit |> (assignLandToUnit landId)

                newLands =
                    model.lands
                        |> List.map
                            (\l ->
                                case l.id == landId of
                                    -- deploy/move unit to Land
                                    True ->
                                        { l | units = (::) landUnit l.units }

                                    -- remove unit from Land
                                    False ->
                                        { l | units = List.filter (\u -> u /= chosenUnit) l.units }
                            )
            in
                { model | lands = newLands }

        CardSpell _ ->
            model

        CardNothing ->
            model


exhaustChosenUnit : Model -> Model
exhaustChosenUnit model =
    let
        chosenUnit =
            model.chosenCard |> unpackCard |> unpackUnit

        newLands =
            model.lands
                |> List.map
                    (\l ->
                        case List.member chosenUnit l.units of
                            True ->
                                let
                                    newUnits =
                                        l.units
                                            |> List.map
                                                (\u ->
                                                    case u == chosenUnit of
                                                        True ->
                                                            { u | exhausted = True }

                                                        False ->
                                                            u
                                                )
                                in
                                    { l | units = newUnits }

                            False ->
                                l
                    )
    in
        { model | lands = newLands }


applyDamageToEnemy : Int -> Model -> Model
applyDamageToEnemy enemyId model =
    let
        chosenUnit =
            model.chosenCard |> unpackCard |> unpackUnit

        newEnemies =
            model.enemies
                |> List.map
                    (\e ->
                        case e.id == enemyId of
                            True ->
                                { e | hp = e.hp - chosenUnit.atk }

                            False ->
                                e
                    )
    in
        { model | enemies = newEnemies }


removeDeadEnemies : Model -> Model
removeDeadEnemies model =
    let
        newEnemies =
            List.filter (\e -> e.hp > 0) model.enemies
    in
        { model | enemies = newEnemies }


updateChosenCard : Maybe Card -> Model -> Model
updateChosenCard card model =
    { model | chosenCard = card }


updateChosenLandId : Maybe Int -> Model -> Model
updateChosenLandId card model =
    { model | chosenLandId = card }


unexhaustUnit : Unit -> Unit
unexhaustUnit unit =
    { unit | exhausted = False }


exhaustUnit : Unit -> Unit
exhaustUnit unit =
    { unit | exhausted = True }


ageUnit : Unit -> Unit
ageUnit unit =
    { unit | age = unit.age + 1 }


assignLandToUnit : Int -> Unit -> Unit
assignLandToUnit landId unit =
    { unit | landId = Just landId }


unpackUnit : Card -> Unit
unpackUnit card =
    case card of
        CardUnit unit ->
            unit

        CardSpell spell ->
            Unit 0 Fighter 0 0 0 0 0 0 False Nothing

        CardNothing ->
            Unit 0 Fighter 0 0 0 0 0 0 False Nothing


unpackCard : Maybe Card -> Card
unpackCard maybeCard =
    maybeCard |> Maybe.withDefault CardNothing


unpackPreview : Maybe Preview -> Preview
unpackPreview maybePreview =
    maybePreview |> Maybe.withDefault PreviewNothing


matureUnit : Unit -> Unit
matureUnit unit =
    case unit.exhausted of
        True ->
            -- don't mature an exhausted unit
            unit

        -- mature unexhausted unit
        False ->
            case unit.kind of
                Fighter ->
                    { unit | atk = unit.atk + 1, hp = unit.hp + 1 }

                -- let
                --     oldMana =
                --         unit.mana
                -- in
                --     case unit.age of
                --         0 ->
                --             { unit | atk = base.fighter.atk, hp = base.fighter.hp }
                --
                --         1 ->
                --             { unit | atk = 6, hp = 20 }
                --
                --         3 ->
                --             { unit | atk = 7, hp = 22 }
                --
                --         5 ->
                --             { unit | atk = 8, hp = 25 }
                --
                --         _ ->
                --             { unit | atk = base.fighter.atk, hp = base.fighter.hp }
                Scout ->
                    { unit | hp = unit.hp + 1 }

                -- let
                --     oldMana =
                --         unit.mana
                -- in
                --     case unit.age of
                --         0 ->
                --             { unit | hp = base.scout.hp }
                --
                --         1 ->
                --             { unit | hp = 11 }
                --
                --         3 ->
                --             { unit | hp = 12 }
                --
                --         5 ->
                --             { unit | hp = 15 }
                --
                --         _ ->
                --             { unit | hp = base.scout.hp }
                Shaman ->
                    { unit | atk = unit.nomi + 1 }



-- let
--     oldMana =
--         unit.mana
-- in
--     case unit.age of
--         0 ->
--             { unit | nomi = base.shaman.nomi }
--
--         1 ->
--             { unit | nomi = 3 }
--
--         3 ->
--             { unit | nomi = 4 }
--
--         5 ->
--             { unit | nomi = 7 }
--
--         _ ->
--             { unit | nomi = base.shaman.nomi }


notEnoughMana : Maybe Card -> Int -> Bool
notEnoughMana chosenCard mana =
    let
        unpackedChosenCard =
            chosenCard
                |> Maybe.withDefault CardNothing
    in
        case unpackedChosenCard of
            CardUnit unit ->
                unit.mana > mana

            CardSpell spell ->
                spell.mana > mana

            CardNothing ->
                False


isExhausted : Maybe Card -> Bool
isExhausted chosenCard =
    let
        unpackedChosenCard =
            chosenCard
                |> Maybe.withDefault CardNothing
    in
        case unpackedChosenCard of
            CardUnit unit ->
                unit.exhausted

            CardSpell spell ->
                False

            CardNothing ->
                False


isDeployed : Maybe Card -> Bool
isDeployed chosenCard =
    let
        unpackedChosenCard =
            chosenCard
                |> Maybe.withDefault CardNothing
    in
        case unpackedChosenCard of
            CardUnit unit ->
                unit.landId /= Nothing

            CardSpell spell ->
                False

            CardNothing ->
                False


landIsntAdjacent : Maybe Card -> Maybe Int -> Bool
landIsntAdjacent chosenCard maybeLandId =
    let
        unpackedChosenCard =
            chosenCard
                |> Maybe.withDefault CardNothing

        chosenCardLandId =
            case unpackedChosenCard of
                CardUnit unit ->
                    unit.landId |> Maybe.withDefault 0

                CardSpell spell ->
                    0

                CardNothing ->
                    0

        unpackedLandId =
            maybeLandId |> Maybe.withDefault 0
    in
        case chosenCardLandId of
            1 ->
                not <| List.member unpackedLandId [ 2, 4 ]

            2 ->
                not <| List.member unpackedLandId [ 1, 3, 5 ]

            3 ->
                not <| List.member unpackedLandId [ 2, 6 ]

            4 ->
                not <| List.member unpackedLandId [ 1, 5 ]

            5 ->
                not <| List.member unpackedLandId [ 4, 2, 6 ]

            6 ->
                not <| List.member unpackedLandId [ 5, 3 ]

            _ ->
                True


tempRandomDeckCards : ( Int, Int ) -> List Card
tempRandomDeckCards ( turn, unitsProduced ) =
    let
        newFighter =
            base.fighter

        newScout =
            base.scout

        newChangeLand =
            base.changeLand
    in
        if List.member turn [ 1, 5, 13, 17, 25, 29 ] then
            [ CardUnit { newScout | id = unitsProduced + 1 } ]
        else if List.member turn [ 9, 21, 33 ] then
            [ CardSpell { newChangeLand | id = unitsProduced + 1 } ]
        else
            [ CardUnit { newFighter | id = unitsProduced + 1 } ]
