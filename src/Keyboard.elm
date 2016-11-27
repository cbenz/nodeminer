module Keyboard exposing (..)


type alias KeyCode =
    Int


type ModifierKey
    = Ctrl
    | Alt
    | Shift


backspace : KeyCode
backspace =
    8


tab : KeyCode
tab =
    9


enter : KeyCode
enter =
    13


shift : KeyCode
shift =
    16


ctrl : KeyCode
ctrl =
    17


alt : KeyCode
alt =
    18


up : KeyCode
up =
    38


down : KeyCode
down =
    40
