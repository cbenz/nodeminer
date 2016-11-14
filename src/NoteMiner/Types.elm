module NoteMiner.Types exposing (..)

import UndoList exposing (UndoList)
import Phoenix.Socket
import MultiwayTreeZipper
import Json.Decode as Decode
import NoteMiner.Tree exposing (NodeId, Tree, Zipper, initialZipper)
import NoteMiner.Maybe exposing (justOrCrash)
import NoteMiner.Keyboard exposing (ModifierKey)


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
    | PhoenixMsg (Phoenix.Socket.Msg Msg)
    | ReceiveChatMessage Decode.Value


type alias Model =
    { treeUndoList : UndoList Tree
    , selectedNodeId : NodeId
    , isAltDown : Bool
    , isCtrlDown : Bool
    , isShiftDown : Bool
    , searchText : String
    , phxSocket : Phoenix.Socket.Socket Msg
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
