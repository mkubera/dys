module View exposing (..)

import Html exposing (Html, Attribute)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Json.Decode as JD
import Style exposing (StyleSheet, hover)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Color exposing (rgba)
import Model exposing (Model, Msg, Msg(..), Card(..), LandKind(..), Preview(..))
import Update.Helpers exposing (unpackCard, unpackPreview)


type Style
    = Wrapper
    | None
    | ErrMsg
    | Enemies
    | Enemy
    | Lands
    | Land
    | EndTurnMana
    | Deck
    | Card
    | ExhaustedCard
    | ModalLandChanged
    | ModalLandChangedBtn
    | CardPreview


stylesheet : StyleSheet Style variation
stylesheet =
    Style.styleSheet
        [ Style.style Wrapper
            [ Font.typeface [ Font.sansSerif ]
            ]
        , Style.style None []
        , Style.style ErrMsg
            [ Color.background (rgba 75 195 131 1.0)
            ]
        , Style.style Enemies
            [ Border.all 0.1
            , Color.border (rgba 0 0 0 1.0)
            ]
        , Style.style Enemy
            [ Color.background (rgba 0 0 0 1.0)
            , Color.text (rgba 255 255 255 1.0)
            ]
        , Style.style Lands
            [ Border.all 0.1
            , Color.border (rgba 0 0 0 1.0)
            ]
        , Style.style Land
            [ Border.all 0.1
            , Color.border (rgba 0 0 0 1.0)
            ]
        , Style.style EndTurnMana
            [ Border.all 0.1
            , Color.border (rgba 0 0 0 1.0)
            ]
        , Style.style Deck
            [ Border.all 0.1
            , Color.border (rgba 0 0 0 1.0)
            ]
        , Style.style Card
            [ Color.background (rgba 132 185 169 1.0)
            ]
        , Style.style ExhaustedCard
            [ Color.background (rgba 132 145 139 1.0)
            ]
        , Style.style ModalLandChanged
            [ Color.background (rgba 255 255 255 0.9)
            ]
        , Style.style ModalLandChangedBtn
            [ Color.background (rgba 0 0 0 1.0)
            , Color.text (rgba 255 255 255 1.0)
            , hover [ Color.background (rgba 55 55 55 1.0) ]
            ]
        , Style.style CardPreview
            [ Color.background (rgba 255 255 255 1.0)
            , Color.text (rgba 0 0 0 1.0)

            -- , hover [ Color.background (rgba 55 55 55 1.0) ]
            ]
        ]


onClickStopPropagation : msg -> Element.Attribute variation msg
onClickStopPropagation msg =
    onWithOptions "click" { stopPropagation = True, preventDefault = False } (JD.succeed msg)


view : Model -> Html Msg
view model =
    layout stylesheet <|
        row Wrapper
            [ width (percent 100), height (percent 100), alignLeft ]
            [ column None
                [ maxWidth (px 500), padding 10, spacing 10 ]
                [ viewErrMsg model.errMsg
                , row None [] [ el None [] (text ("TURN: " ++ toString model.turn)) ]
                , column Enemies
                    [ width fill, padding 10, spacing 10 ]
                    [ el None [] (text "ENEMIES")
                    , wrappedRow None [ spacing 10 ] (List.map viewEnemy model.enemies)
                    ]
                , column Lands
                    [ width fill, padding 10, spacing 10 ]
                    [ el None [] (text "LANDS")
                    , wrappedRow None [ spacing 10 ] (List.map viewLand model.lands)
                    ]
                , row EndTurnMana
                    [ width fill, padding 10, spacing 10 ]
                    [ el None [] (text (""))
                    , row None
                        []
                        [ button None [ padding 5, onClick EndTurn ] (text "End Turn")
                        ]
                    , row None
                        [ padding 5 ]
                        [ el None [] (text ("mana: " ++ (toString model.mana) ++ " / " ++ (toString model.maxMana)))
                        ]
                    ]
                , column Deck
                    [ width fill, padding 10, spacing 10 ]
                    [ row None [] [ el None [] (text ("DECK")) ]
                    , row None [ spacing 2 ] (List.map viewDeckCard model.deck)
                    ]
                , row None [] [ el None [] (text (toString model.chosenCard)) ]

                -- invisibles
                , viewStartGame model.state.gameStarted
                , viewModals model.modals
                ]
            , viewCardPreview model.previewed
            ]


viewEnemy enemy =
    let
        previewed =
            PreviewEnemy enemy
    in
        el Enemy
            [ onClick (UnitAttackEnemy enemy.id)
            , onMouseEnter (TogglePreview previewed)
            , onMouseLeave (TogglePreview previewed)
            , padding 5
            ]
            (text (toString enemy.kind))


viewLand land =
    column Land
        [ onClick (UseCard (Just land.id)), padding 5, width (percent 33.33333333333) ]
        [ row None
            []
            [ el None [] (text ("#" ++ (toString land.id) ++ " " ++ (toString land.kind |> String.toUpper)))
            ]
        , wrappedRow None [] (List.map viewLandUnit land.units)
        ]


viewLandUnit unit =
    let
        style_ =
            case unit.exhausted of
                True ->
                    ExhaustedCard

                False ->
                    Card

        -- tempData =
        --     [ (toString unit.kind), " #", (toString unit.id), " (", (toString unit.atk), " ", (toString unit.hp), " ", (toString unit.age), ")" ]
        --         |> List.foldr (++) ""
        tempData2 =
            (toString unit.kind)

        card =
            CardUnit unit

        previewed =
            PreviewUnit unit
    in
        el style_
            [ onClickStopPropagation (ToggleCard card)
            , onMouseEnter (TogglePreview previewed)
            , onMouseLeave (TogglePreview previewed)
            , padding 5
            ]
            (text tempData2)


viewDeckCard card =
    let
        view_ =
            case card of
                CardUnit unit ->
                    el Card
                        [ onClick (ToggleCard card)
                        , onMouseEnter (TogglePreview (PreviewUnit unit))
                        , onMouseLeave (TogglePreview (PreviewUnit unit))
                        , padding 5
                        ]
                        -- (text ((toString unit.kind) ++ " #" ++ (toString unit.id) ++ " (" ++ (toString unit.mana) ++ ")"))
                        (text (toString unit.kind))

                CardSpell spell ->
                    el Card
                        [ onClick (ToggleCard card)
                        , onMouseEnter (TogglePreview (PreviewSpell spell))
                        , onMouseLeave (TogglePreview (PreviewSpell spell))
                        , padding 5
                        ]
                        -- (text ((toString spell.kind) ++ " #" ++ (toString spell.id) ++ " (" ++ (toString spell.mana) ++ ")"))
                        (text (toString spell.kind))

                CardNothing ->
                    el None [] (text "")
    in
        view_


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


viewModals modals =
    if modals.landChanged then
        modal ModalLandChanged
            [ width (percent 100)
            , height (percent 100)
            , (inlineStyle [ ( "textAlign", "center" ), ( "paddingTop", "45vh" ) ])
            ]
            (row
                None
                [ width fill, spacing 5, center ]
                [ button ModalLandChangedBtn [ onClick (ChangeChosenLand Empty), padding 10 ] (text "Empty")
                , button ModalLandChangedBtn [ onClick (ChangeChosenLand Forest), padding 10 ] (text "Forest")
                , button ModalLandChangedBtn [ onClick (ChangeChosenLand Reinforced), padding 10 ] (text "Reinforced")
                , button ModalLandChangedBtn [ onClick (ChangeChosenLand Outpost), padding 10 ] (text "Outpost")
                ]
            )
    else
        el None [] (text "")


viewCardPreview previewed =
    case unpackPreview previewed of
        PreviewUnit unit ->
            row None
                []
                [ column CardPreview
                    [ width (px 140), height (px 200) ]
                    [ el None [] (text ((toString unit.kind) ++ " #" ++ (toString unit.id)))
                    , el None [] (text ("atk: " ++ (toString unit.atk)))
                    , el None [] (text ("hp: " ++ (toString unit.hp)))
                    , el None [] (text ("mana: " ++ (toString unit.mana)))
                    ]
                ]

        PreviewSpell spell ->
            row None
                []
                [ wrappedColumn CardPreview
                    [ width (px 140), height (px 200) ]
                    [ el None [] (text ((toString spell.kind) ++ " #" ++ (toString spell.id)))
                    , el None [] (text ("mana: " ++ (toString spell.mana)))
                    , el None [] (text (toString spell.description))
                    ]
                ]

        PreviewEnemy enemy ->
            row None
                []
                [ wrappedColumn CardPreview
                    [ width (px 140), height (px 200) ]
                    [ el None [] (text ((toString enemy.kind) ++ " #" ++ (toString enemy.id)))
                    , el None [] (text ("atk: " ++ (toString enemy.atk)))
                    , el None [] (text ("hp: " ++ (toString enemy.hp)))
                    ]
                ]

        PreviewNothing ->
            el CardPreview [ width (px 140), height (px 200) ] (text "No card")
