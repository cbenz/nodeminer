module Types exposing (..)

import Keyboard exposing (..)
import Maybe.Custom exposing (justOrCrash)
import MultiwayTreeZipper
import Tree exposing (NodeId, Tree, Zipper, initialZipper)
import UndoList exposing (UndoList)


type alias Model =
    { treeUndoList : UndoList Tree
    , selectedNodeId : NodeId
    , isAltDown : Bool
    , isCtrlDown : Bool
    , isShiftDown : Bool
    , searchText : String
    }


type Msg
    = NoOp
    | ChangeModifierKey ModifierKey Bool
    | Undo
    | Redo
    | ResetToSampleTree
    | SelectNode NodeId
    | SetText String
    | SelectPreviousNode
    | SelectNextNode
    | InsertNodeBelow
    | RemoveCurrentNode
    | RemoveCurrentNodeFromBackspace
    | IndentCurrentNode
    | DedentCurrentNode
    | MoveCurrentNodeUp
    | MoveCurrentNodeDown
    | SetSearchText String


type alias SerializedTree =
    String


getSelectedNodeZipper : Model -> Zipper
getSelectedNodeZipper model =
    let
        tree =
            model.treeUndoList.present
    in
        initialZipper tree
            |> MultiwayTreeZipper.goTo (\datum -> datum.id == model.selectedNodeId)
            |> justOrCrash "getSelectedNodeZipper"
