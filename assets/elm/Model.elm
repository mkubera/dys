module Model exposing (..)


type Msg
    = StartGame
    | NoOfOutposts Int
    | MoveDiceToOutpost Int
    | ToggleDie Die
    | SaveDice (List Die)
    | EndTurn
    | NoOp


type DiceKind
    = Fighter
    | Scout
    | Grower
    | Dreamer


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


type alias State =
    { gameStarted : Bool
    }


type alias Settings =
    { noOfOutposts : Int
    }


type alias Outpost =
    { id : Int, dice : List Die }


type alias Model =
    { state : State
    , settings : Settings
    , outposts : List Outpost
    , savedDice : List Die
    , rolledDice : List Die
    , turn : Int
    , unitsProduced : Int
    }


type alias Die =
    { id : Int, kind : DiceKind, age : Int, atk : Int, hp : Int, nomi : Int, goe : Int, chosen : Bool }


base : { fighter : Die, scout : Die, grower : Die, dreamer : Die }
base =
    { fighter = { id = 0, kind = Fighter, age = 0, atk = 5, hp = 20, nomi = 1, goe = 0, chosen = False }
    , scout = { id = 0, kind = Scout, age = 0, atk = 2, hp = 10, nomi = 1, goe = 0, chosen = False }
    , grower = { id = 0, kind = Grower, age = 0, atk = 1, hp = 8, nomi = 2, goe = 0, chosen = False }
    , dreamer = { id = 0, kind = Dreamer, age = 0, atk = 0, hp = 6, nomi = 10, goe = 0, chosen = False }
    }


initState : State
initState =
    { gameStarted = False }


initSettings : Settings
initSettings =
    { noOfOutposts = 3 }


initDice : List Die
initDice =
    let
        baseFighter =
            base.fighter

        newFighter =
            { baseFighter | id = 1 }
    in
        [ newFighter ]


initModel : Model
initModel =
    Model initState initSettings [] [] initDice 1 1


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )
