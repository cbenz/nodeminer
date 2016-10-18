module NoteMiner.Model exposing (..)

import UndoList exposing (UndoList)
import NoteMiner.Tree exposing (NodeId, Tree, Zipper, initialZipper)
import NoteMiner.Maybe exposing (justOrCrash)
import MultiwayTreeZipper


type alias Model =
    { treeUndoList : UndoList Tree
    , selectedNodeId : NodeId
    , isAltDown : Bool
    , isCtrlDown : Bool
    , isShiftDown : Bool
    , searchText : String
    }


getSelectedNodeZipper : Model -> Zipper
getSelectedNodeZipper model =
    let
        tree =
            model.treeUndoList.present
    in
        initialZipper tree
            |> MultiwayTreeZipper.goTo (\datum -> datum.id == model.selectedNodeId)
            |> justOrCrash "getSelectedNodeZipper"
