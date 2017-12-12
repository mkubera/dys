module Model exposing (..)

import Task


type Msg
    = StartGame Bool
    | NoOfOutposts Int
    | ToggleDie Die
    | MoveDice (Location Int)
    | EndTurn
    | NoOp


type DieKind
    = Fighter
    | Scout
    | Shaman


type LandKind
    = Empty
    | Outpost
    | Forest
    | Heart


type alias State =
    { gameStarted : Bool
    }


type alias Settings =
    { noOfOutposts : Int
    }


type alias Model =
    { state : State
    , settings : Settings
    , outposts : List Land
    , savedDice : List Die
    , rolledDice : List Die
    , mana : Int
    , maxMana : Int
    , turn : Int
    , unitsProduced : Int
    , errMsg : Maybe String
    , chosenCard : Maybe Die
    }


type Location a
    = LocationRolled
    | LocationSaved
    | LocationOutpost a


type Card
    = CardDie Die
    | CardSpell Spell


type alias Die =
    { id : Int, kind : DieKind, age : Int, atk : Int, hp : Int, nomi : Int, goe : Int, mana : Mana, location : Location Int, chosen : Bool, exhausted : Bool }


type alias Spell =
    { kind : SpellKind, mana : Int, description : String }


type SpellKind
    = ChangeLand


type alias Land =
    { id : Int
    , kind : LandKind
    , dice : List Die
    , cardChosen : Bool
    }


type alias Mana =
    { rolledToSaved : Int
    , rolledToOutpost : Int
    , savedToOutpost : Int
    }


base : { fighter : Die, scout : Die, shaman : Die }
base =
    { fighter =
        { id = 0
        , kind = Fighter
        , age = 0
        , atk = 5
        , hp = 20
        , nomi = 1
        , goe = 0
        , mana =
            { rolledToSaved = 1
            , rolledToOutpost = 2
            , savedToOutpost = 1
            }
        , location = LocationRolled
        , chosen = False
        , exhausted = False
        }
    , scout =
        { id = 0
        , kind = Scout
        , age = 0
        , atk = 2
        , hp = 10
        , nomi = 1
        , goe = 0
        , mana =
            { rolledToSaved = 0
            , rolledToOutpost = 1
            , savedToOutpost = 1
            }
        , location = LocationRolled
        , chosen = False
        , exhausted = False
        }
    , shaman =
        { id = 0
        , kind = Shaman
        , age = 0
        , atk = 1
        , hp = 8
        , nomi = 2
        , goe = 0
        , mana =
            { rolledToSaved = 1
            , rolledToOutpost = 2
            , savedToOutpost = 1
            }
        , location = LocationRolled
        , chosen = False
        , exhausted = False
        }
    }


initState : State
initState =
    { gameStarted = False }


initSettings : Settings
initSettings =
    { noOfOutposts = 5 }


initOutposts =
    [ Land 1 Empty [] False
    , Land 2 Empty [] False
    , Land 3 Empty [] False
    , Land 4 Empty [] False
    , Land 5 Heart [] False
    , Land 6 Empty [] False
    ]


initSavedDice =
    []


initRolledDice : List Die
initRolledDice =
    []


initUnitsProduced =
    3


initModel : Model
initModel =
    Model initState initSettings initOutposts initSavedDice initRolledDice 1 1 1 initUnitsProduced Nothing Nothing


initCmds =
    Task.perform StartGame (Task.succeed True)


init : ( Model, Cmd Msg )
init =
    ( initModel, initCmds )
