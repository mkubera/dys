module Model exposing (..)

import Task


type Msg
    = StartGame Bool
    | ToggleCard Card
    | TogglePreview Preview
    | UseCard (Maybe Int)
    | UnitAttackEnemy Int
    | ChangeChosenLand LandKind
    | EndTurn
    | NoOp


type UnitKind
    = Fighter
    | Scout
    | Shaman


type LandKind
    = Empty
    | Outpost
    | Reinforced
    | Forest
    | Heart


type alias State =
    { gameStarted : Bool
    }


type alias Modals =
    { landChanged : Bool
    }


type alias Model =
    { state : State
    , enemies : List Enemy
    , lands : List Land
    , deck : List Card
    , mana : Int
    , maxMana : Int
    , turn : Int
    , unitsProduced : Int
    , errMsg : Maybe String
    , chosenCard : Maybe Card
    , previewed : Maybe Preview
    , chosenLandId : Maybe Int
    , modals : Modals
    }


type alias Land =
    { id : Int
    , kind : LandKind
    , units : List Unit
    }


type Card
    = CardUnit Unit
    | CardSpell Spell
    | CardNothing


type Preview
    = PreviewUnit Unit
    | PreviewSpell Spell
    | PreviewEnemy Enemy
    | PreviewNothing


type alias Unit =
    { id : Int, kind : UnitKind, age : Int, atk : Int, hp : Int, nomi : Int, goe : Int, mana : Int, exhausted : Bool, landId : Maybe Int }


type alias Spell =
    { id : Int, kind : SpellKind, mana : Int, description : String }


type SpellKind
    = ChangeLand


type alias Enemy =
    { id : Int, kind : EnemyKind, atk : Int, hp : Int }


type EnemyKind
    = SystemSpawn
    | ShadowlessOppressor


base : { fighter : Unit, scout : Unit, shaman : Unit, changeLand : Spell, systemSpawn : Enemy, shadowlessOppressor : Enemy }
base =
    { fighter =
        { id = 0
        , kind = Fighter
        , age = 0
        , atk = 5
        , hp = 20
        , nomi = 1
        , goe = 0
        , mana = 2
        , exhausted = False
        , landId = Nothing
        }
    , scout =
        { id = 0
        , kind = Scout
        , age = 0
        , atk = 2
        , hp = 10
        , nomi = 1
        , goe = 0
        , mana = 1
        , exhausted = False
        , landId = Nothing
        }
    , shaman =
        { id = 0
        , kind = Shaman
        , age = 0
        , atk = 1
        , hp = 8
        , nomi = 2
        , goe = 0
        , mana = 3
        , exhausted = False
        , landId = Nothing
        }
    , changeLand =
        { id = 0
        , kind = ChangeLand
        , mana = 2
        , description = "Transform a land into Forest, Outpost, or Empty."
        }
    , systemSpawn =
        { id = 0
        , kind = SystemSpawn
        , atk = 2
        , hp = 4
        }
    , shadowlessOppressor =
        { id = 0
        , kind = ShadowlessOppressor
        , atk = 6
        , hp = 3
        }
    }


initState : State
initState =
    { gameStarted = False }


initEnemies : List Enemy
initEnemies =
    let
        baseSystemSpawn =
            base.systemSpawn

        baseShadowlessOppressor =
            base.shadowlessOppressor

        systemSpawn1 =
            { baseSystemSpawn | id = 1 }

        systemSpawn2 =
            { baseSystemSpawn | id = 2 }

        systemSpawn3 =
            { baseSystemSpawn | id = 3 }

        shadowlessOppressor1 =
            { baseShadowlessOppressor | id = 2 }
    in
        [ systemSpawn1, shadowlessOppressor1, systemSpawn3 ]


initLandHeartId : Int
initLandHeartId =
    5


initLands : List Land
initLands =
    [ Land 1 Empty []
    , Land 2 Empty []
    , Land 3 Empty []
    , Land 4 Empty []
    , Land initLandHeartId Heart []
    , Land 6 Empty []
    ]


initDeck : List Card
initDeck =
    []


initUnitsProduced : Int
initUnitsProduced =
    3


initModals : Modals
initModals =
    Modals False


initModel : Model
initModel =
    Model initState initEnemies initLands initDeck 1 1 1 initUnitsProduced Nothing Nothing Nothing Nothing initModals


initCmds : Cmd Msg
initCmds =
    Task.perform StartGame (Task.succeed True)


init : ( Model, Cmd Msg )
init =
    ( initModel, initCmds )
