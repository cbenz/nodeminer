port module NoteMiner.Storage exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode
import MultiwayTree
import NoteMiner.Model exposing (Model)
import NoteMiner.Update as Update exposing (Msg)
import NoteMiner.Tree exposing (Datum, Tree, node)
import NoteMiner.SerializedTree exposing (SerializedTree)


-- PORT


port setStorage : SerializedTree -> Cmd msg



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, cmds ) =
            Update.update msg model

        tree =
            newModel.treeUndoList.present
    in
        ( newModel
        , Cmd.batch [ setStorage (serialize tree), cmds ]
        )



-- ENCODERS


serialize : Tree -> String
serialize tree =
    Encode.encode 0 (encodeTreeNode tree)


encodeTreeNode : Tree -> Encode.Value
encodeTreeNode node =
    Encode.object
        [ ( "datum", encodeDatum (MultiwayTree.datum node) )
        , ( "children", Encode.list (List.map encodeTreeNode (MultiwayTree.children node)) )
        ]


encodeDatum : Datum -> Encode.Value
encodeDatum datum =
    Encode.object [ ( "text", Encode.string datum.text ), ( "id", Encode.int datum.id ) ]



-- DECODERS


type DatumPair
    = DatumPair Datum (List DatumPair)


datumPairToTree : DatumPair -> Tree
datumPairToTree (DatumPair datum pairs) =
    node datum.text datum.id (List.map datumPairToTree pairs)


unserialize : String -> Result String Tree
unserialize str =
    str
        |> Decode.decodeString decodeTreeNode
        |> Result.map datumPairToTree


decodeTreeNode : Decode.Decoder DatumPair
decodeTreeNode =
    Decode.map2
        DatumPair
        (Decode.field "datum" decodeDatum)
        (Decode.field "children" (Decode.lazy (\_ -> Decode.list decodeTreeNode)))


decodeDatum : Decode.Decoder Datum
decodeDatum =
    Decode.map2
        Datum
        (Decode.field "text" Decode.string)
        (Decode.field "id" Decode.int)
