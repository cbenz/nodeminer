module NoteMiner.Update exposing (..)

import String
import Dom
import Task exposing (Task)
import UndoList
import MultiwayTreeZipper
import NoteMiner.Keyboard exposing (ModifierKey(..))
import NoteMiner.Tree exposing (..)
import NoteMiner.SampleData
import NoteMiner.Model exposing (Model, getSelectedNodeZipper)
import NoteMiner.Maybe exposing (justOrCrash)
import NoteMiner.Constants exposing (selectedNodeIdHtmlAttribute)


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        tree =
            model.treeUndoList.present

        selectedNodeZipper =
            getSelectedNodeZipper model

        focusCmd =
            Task.attempt (\_ -> NoOp) (Dom.focus selectedNodeIdHtmlAttribute)
    in
        case msg of
            NoOp ->
                ( model, Cmd.none )

            ChangeModifierKey modifierKey isPressed ->
                case modifierKey of
                    Alt ->
                        ( { model | isAltDown = isPressed }, Cmd.none )

                    Ctrl ->
                        ( { model | isCtrlDown = isPressed }, Cmd.none )

                    Shift ->
                        ( { model | isShiftDown = isPressed }, Cmd.none )

            SelectNode nodeId ->
                ( { model | selectedNodeId = nodeId }
                , focusCmd
                )

            SetText text ->
                let
                    tree_ =
                        MultiwayTreeZipper.updateDatum
                            (\datum -> { datum | text = text })
                            selectedNodeZipper
                            |> justOrCrash "SetText"
                            |> getTreeRootFromZipper

                    treeUndoList_ =
                        if String.endsWith " " text then
                            UndoList.new tree_ model.treeUndoList
                        else
                            UndoList.mapPresent (always tree_) model.treeUndoList
                in
                    ( { model | treeUndoList = treeUndoList_ }
                    , Cmd.none
                    )

            SelectPreviousNode ->
                let
                    selectedNodeId_ =
                        (if canGoToPrevious selectedNodeZipper then
                            MultiwayTreeZipper.goToPrevious selectedNodeZipper
                                |> justOrCrash "updateSelectPreviousNode"
                         else
                            selectedNodeZipper
                        )
                            |> MultiwayTreeZipper.datum
                            |> .id
                in
                    ( { model | selectedNodeId = selectedNodeId_ }
                    , focusCmd
                    )

            SelectNextNode ->
                let
                    selectedNodeId_ =
                        (if canGoToNext selectedNodeZipper then
                            MultiwayTreeZipper.goToNext selectedNodeZipper
                                |> justOrCrash "updateSelectNextNode"
                         else
                            selectedNodeZipper
                        )
                            |> MultiwayTreeZipper.datum
                            |> .id
                in
                    ( { model | selectedNodeId = selectedNodeId_ }
                    , focusCmd
                    )

            InsertNodeBelow ->
                let
                    newNode =
                        nextNode tree

                    selectedNodeZipper_ =
                        if hasChildren selectedNodeZipper then
                            MultiwayTreeZipper.insertChild newNode selectedNodeZipper
                        else
                            insertSiblingBelow newNode selectedNodeZipper

                    treeUndoList_ =
                        selectedNodeZipper_
                            |> Maybe.andThen MultiwayTreeZipper.goToRoot
                            |> justOrCrash "updateInsertNodeBelow: treeUndoList'"
                            |> Tuple.first
                            |> \tree -> UndoList.new tree model.treeUndoList

                    selectedNodeId_ =
                        selectedNodeZipper_
                            |> Maybe.andThen
                                (if hasChildren selectedNodeZipper then
                                    MultiwayTreeZipper.goToChild 0
                                 else
                                    MultiwayTreeZipper.goToNext
                                )
                            |> justOrCrash "updateInsertNodeBelow: selectedNodeId'"
                            |> MultiwayTreeZipper.datum
                            |> .id
                in
                    ( { model
                        | treeUndoList = treeUndoList_
                        , selectedNodeId = selectedNodeId_
                      }
                    , focusCmd
                    )

            IndentCurrentNode ->
                let
                    index =
                        findIndexInSiblings selectedNodeZipper
                in
                    if canIndent selectedNodeZipper then
                        let
                            treeUndoList_ =
                                removeCurrentAndGoUp selectedNodeZipper
                                    |> Maybe.andThen (MultiwayTreeZipper.goToChild (index - 1))
                                    |> Maybe.andThen (MultiwayTreeZipper.appendChild (Tuple.first selectedNodeZipper))
                                    |> Maybe.andThen MultiwayTreeZipper.goToRoot
                                    |> justOrCrash "updateIndentCurrentNode"
                                    |> Tuple.first
                                    |> \tree -> UndoList.new tree model.treeUndoList
                        in
                            ( { model | treeUndoList = treeUndoList_ }
                            , focusCmd
                            )
                    else
                        -- To be indented the node must have at least a previous sibling.
                        ( model, Cmd.none )

            DedentCurrentNode ->
                if canDedent selectedNodeZipper then
                    let
                        treeUndoList_ =
                            removeCurrentAndGoUp selectedNodeZipper
                                |> Maybe.andThen (insertSiblingBelow (Tuple.first selectedNodeZipper))
                                |> Maybe.andThen MultiwayTreeZipper.goToRoot
                                |> justOrCrash "updateDedentCurrentNode"
                                |> Tuple.first
                                |> \tree -> UndoList.new tree model.treeUndoList
                    in
                        ( { model | treeUndoList = treeUndoList_ }
                        , focusCmd
                        )
                else
                    -- To be dedented the node must have a depth > 1.
                    ( model, Cmd.none )

            MoveCurrentNodeUp ->
                if canMoveUp selectedNodeZipper then
                    let
                        index =
                            findIndexInSiblings selectedNodeZipper

                        index_ =
                            index - 1

                        treeUndoList_ =
                            removeCurrentAndGoUp selectedNodeZipper
                                |> Maybe.andThen (insertChildAtIndex (Tuple.first selectedNodeZipper) index_)
                                |> Maybe.andThen MultiwayTreeZipper.goToRoot
                                |> justOrCrash "updateMoveCurrentNodeUp"
                                |> Tuple.first
                                |> \tree -> UndoList.new tree model.treeUndoList
                    in
                        ( { model | treeUndoList = treeUndoList_ }
                        , focusCmd
                        )
                else
                    ( model, Cmd.none )

            MoveCurrentNodeDown ->
                if canMoveDown selectedNodeZipper then
                    let
                        index =
                            findIndexInSiblings selectedNodeZipper

                        index_ =
                            index + 1

                        treeUndoList_ =
                            removeCurrentAndGoUp selectedNodeZipper
                                |> Maybe.andThen (insertChildAtIndex (Tuple.first selectedNodeZipper) index_)
                                |> Maybe.andThen MultiwayTreeZipper.goToRoot
                                |> justOrCrash "updateMoveCurrentNodeDown"
                                |> Tuple.first
                                |> \tree -> UndoList.new tree model.treeUndoList
                    in
                        ( { model | treeUndoList = treeUndoList_ }
                        , focusCmd
                        )
                else
                    ( model, Cmd.none )

            RemoveCurrentNode ->
                if canRemove selectedNodeZipper then
                    let
                        selectedNodeZipper_ =
                            removeCurrentAndGoUp selectedNodeZipper

                        treeUndoList_ =
                            selectedNodeZipper_
                                |> Maybe.andThen MultiwayTreeZipper.goToRoot
                                |> justOrCrash "updateRemoveCurrentNode: treeUndoList'"
                                |> Tuple.first
                                |> \tree -> UndoList.new tree model.treeUndoList

                        selectedNodeId_ =
                            selectedNodeZipper_
                                |> Maybe.andThen
                                    (\parent ->
                                        if hasChildren parent then
                                            if isLastSibling selectedNodeZipper then
                                                MultiwayTreeZipper.goToRightMostChild parent
                                            else
                                                let
                                                    index =
                                                        findIndexInSiblings selectedNodeZipper
                                                in
                                                    MultiwayTreeZipper.goToChild index parent
                                        else
                                            Just parent
                                    )
                                |> justOrCrash "updateRemoveCurrentNode: selectedNodeId'"
                                |> MultiwayTreeZipper.datum
                                |> .id
                    in
                        ( { model
                            | treeUndoList = treeUndoList_
                            , selectedNodeId = selectedNodeId_
                          }
                        , focusCmd
                        )
                else
                    ( model, Cmd.none )

            RemoveCurrentNodeFromBackspace ->
                if canRemove selectedNodeZipper then
                    let
                        selectedNodeZipper_ =
                            removeCurrentAndGoUp selectedNodeZipper

                        treeUndoList_ =
                            selectedNodeZipper_
                                |> Maybe.andThen MultiwayTreeZipper.goToRoot
                                |> justOrCrash "updateRemoveCurrentNode: treeUndoList'"
                                |> Tuple.first
                                |> \tree -> UndoList.new tree model.treeUndoList

                        selectedNodeId_ =
                            selectedNodeZipper_
                                |> Maybe.andThen
                                    (\parent ->
                                        if hasChildren parent then
                                            let
                                                index =
                                                    findIndexInSiblings selectedNodeZipper
                                            in
                                                MultiwayTreeZipper.goToChild (max 0 (index - 1)) parent
                                        else
                                            Just parent
                                    )
                                |> justOrCrash "RemoveCurrentNodeFromBackspace"
                                |> MultiwayTreeZipper.datum
                                |> .id
                    in
                        ( { model
                            | treeUndoList = treeUndoList_
                            , selectedNodeId = selectedNodeId_
                          }
                        , focusCmd
                        )
                else
                    ( model, Cmd.none )

            ResetToSampleTree ->
                let
                    treeUndoList_ =
                        MultiwayTreeZipper.goToChild 0 (initialZipper NoteMiner.SampleData.sampleTree)
                            |> justOrCrash "ResetToSampleTree"
                            |> getTreeRootFromZipper
                            |> \tree -> UndoList.fresh tree
                in
                    ( { model | treeUndoList = treeUndoList_ }
                    , focusCmd
                    )

            Undo ->
                let
                    treeUndoList_ =
                        UndoList.undo model.treeUndoList
                in
                    ( { model | treeUndoList = treeUndoList_ }
                    , focusCmd
                    )

            Redo ->
                let
                    treeUndoList_ =
                        UndoList.redo model.treeUndoList
                in
                    ( { model | treeUndoList = treeUndoList_ }
                    , focusCmd
                    )

            SetSearchText text ->
                ( { model | searchText = text }, Cmd.none )
