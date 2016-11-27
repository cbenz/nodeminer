module NoteMiner exposing (..)

import Html
import Json.Decode as Decode
import State
import Storage
import Types exposing (..)
import Views


-- MAIN


main : Program Decode.Value Model Msg
main =
    Html.programWithFlags
        { init = State.init
        , view = Views.root
        , update = Storage.update State.update
        , subscriptions = always Sub.none
        }
