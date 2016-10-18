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


performBlind : Task a b -> Cmd Msg
performBlind =
    Task.perform (always NoOp) (always NoOp)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        tree =
            model.treeUndoList.present

        selectedNodeZipper =
            getSelectedNodeZipper model
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
                , performBlind (Dom.focus selectedNodeIdHtmlAttribute)
                )

            SetText text ->
                let
                    tree' =
                        MultiwayTreeZipper.updateDatum
                            (\datum -> { datum | text = text })
                            selectedNodeZipper
                            |> justOrCrash "SetText"
                            |> getTreeRootFromZipper

                    treeUndoList' =
                        if String.endsWith " " text then
                            UndoList.new tree' model.treeUndoList
                        else
                            UndoList.mapPresent (always tree') model.treeUndoList
                in
                    ( { model | treeUndoList = treeUndoList' }
                    , Cmd.none
                    )

            SelectPreviousNode ->
                let
                    selectedNodeId' =
                        (if canGoToPrevious selectedNodeZipper then
                            MultiwayTreeZipper.goToPrevious selectedNodeZipper
                                |> justOrCrash "updateSelectPreviousNode"
                         else
                            selectedNodeZipper
                        )
                            |> MultiwayTreeZipper.datum
                            |> .id
                in
                    ( { model | selectedNodeId = selectedNodeId' }
                    , performBlind (Dom.focus selectedNodeIdHtmlAttribute)
                    )

            SelectNextNode ->
                let
                    selectedNodeId' =
                        (if canGoToNext selectedNodeZipper then
                            MultiwayTreeZipper.goToNext selectedNodeZipper
                                |> justOrCrash "updateSelectNextNode"
                         else
                            selectedNodeZipper
                        )
                            |> MultiwayTreeZipper.datum
                            |> .id
                in
                    ( { model | selectedNodeId = selectedNodeId' }
                    , performBlind (Dom.focus selectedNodeIdHtmlAttribute)
                    )

            InsertNodeBelow ->
                let
                    newNode =
                        nextNode tree

                    selectedNodeZipper' =
                        if hasChildren selectedNodeZipper then
                            MultiwayTreeZipper.insertChild newNode selectedNodeZipper
                        else
                            insertSiblingBelow newNode selectedNodeZipper

                    treeUndoList' =
                        selectedNodeZipper'
                            `Maybe.andThen` MultiwayTreeZipper.goToRoot
                            |> justOrCrash "updateInsertNodeBelow: treeUndoList'"
                            |> fst
                            |> \tree -> UndoList.new tree model.treeUndoList

                    selectedNodeId' =
                        selectedNodeZipper'
                            `Maybe.andThen`
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
                        | treeUndoList = treeUndoList'
                        , selectedNodeId = selectedNodeId'
                      }
                    , performBlind (Dom.focus selectedNodeIdHtmlAttribute)
                    )

            IndentCurrentNode ->
                let
                    index =
                        findIndexInSiblings selectedNodeZipper
                in
                    if canIndent selectedNodeZipper then
                        let
                            treeUndoList' =
                                removeCurrentAndGoUp selectedNodeZipper
                                    `Maybe.andThen` MultiwayTreeZipper.goToChild (index - 1)
                                    `Maybe.andThen` MultiwayTreeZipper.appendChild (fst selectedNodeZipper)
                                    `Maybe.andThen` MultiwayTreeZipper.goToRoot
                                    |> justOrCrash "updateIndentCurrentNode"
                                    |> fst
                                    |> \tree -> UndoList.new tree model.treeUndoList
                        in
                            ( { model | treeUndoList = treeUndoList' }
                            , performBlind (Dom.focus selectedNodeIdHtmlAttribute)
                            )
                    else
                        -- To be indented the node must have at least a previous sibling.
                        ( model, Cmd.none )

            DedentCurrentNode ->
                if canDedent selectedNodeZipper then
                    let
                        treeUndoList' =
                            removeCurrentAndGoUp selectedNodeZipper
                                `Maybe.andThen` insertSiblingBelow (fst selectedNodeZipper)
                                `Maybe.andThen` MultiwayTreeZipper.goToRoot
                                |> justOrCrash "updateDedentCurrentNode"
                                |> fst
                                |> \tree -> UndoList.new tree model.treeUndoList
                    in
                        ( { model | treeUndoList = treeUndoList' }
                        , performBlind (Dom.focus selectedNodeIdHtmlAttribute)
                        )
                else
                    -- To be dedented the node must have a depth > 1.
                    ( model, Cmd.none )

            MoveCurrentNodeUp ->
                if canMoveUp selectedNodeZipper then
                    let
                        index =
                            findIndexInSiblings selectedNodeZipper

                        index' =
                            index - 1

                        treeUndoList' =
                            removeCurrentAndGoUp selectedNodeZipper
                                `Maybe.andThen` insertChildAtIndex (fst selectedNodeZipper) index'
                                `Maybe.andThen` MultiwayTreeZipper.goToRoot
                                |> justOrCrash "updateMoveCurrentNodeUp"
                                |> fst
                                |> \tree -> UndoList.new tree model.treeUndoList
                    in
                        ( { model | treeUndoList = treeUndoList' }
                        , performBlind (Dom.focus selectedNodeIdHtmlAttribute)
                        )
                else
                    ( model, Cmd.none )

            MoveCurrentNodeDown ->
                if canMoveDown selectedNodeZipper then
                    let
                        index =
                            findIndexInSiblings selectedNodeZipper

                        index' =
                            index + 1

                        treeUndoList' =
                            removeCurrentAndGoUp selectedNodeZipper
                                `Maybe.andThen` insertChildAtIndex (fst selectedNodeZipper) index'
                                `Maybe.andThen` MultiwayTreeZipper.goToRoot
                                |> justOrCrash "updateMoveCurrentNodeDown"
                                |> fst
                                |> \tree -> UndoList.new tree model.treeUndoList
                    in
                        ( { model | treeUndoList = treeUndoList' }
                        , performBlind (Dom.focus selectedNodeIdHtmlAttribute)
                        )
                else
                    ( model, Cmd.none )

            RemoveCurrentNode ->
                if canRemove selectedNodeZipper then
                    let
                        selectedNodeZipper' =
                            removeCurrentAndGoUp selectedNodeZipper

                        treeUndoList' =
                            selectedNodeZipper'
                                `Maybe.andThen` MultiwayTreeZipper.goToRoot
                                |> justOrCrash "updateRemoveCurrentNode: treeUndoList'"
                                |> fst
                                |> \tree -> UndoList.new tree model.treeUndoList

                        selectedNodeId' =
                            selectedNodeZipper'
                                `Maybe.andThen`
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
                            | treeUndoList = treeUndoList'
                            , selectedNodeId = selectedNodeId'
                          }
                        , performBlind (Dom.focus selectedNodeIdHtmlAttribute)
                        )
                else
                    ( model, Cmd.none )

            RemoveCurrentNodeFromBackspace ->
                if canRemove selectedNodeZipper then
                    let
                        selectedNodeZipper' =
                            removeCurrentAndGoUp selectedNodeZipper

                        treeUndoList' =
                            selectedNodeZipper'
                                `Maybe.andThen` MultiwayTreeZipper.goToRoot
                                |> justOrCrash "updateRemoveCurrentNode: treeUndoList'"
                                |> fst
                                |> \tree -> UndoList.new tree model.treeUndoList

                        selectedNodeId' =
                            selectedNodeZipper'
                                `Maybe.andThen`
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
                            | treeUndoList = treeUndoList'
                            , selectedNodeId = selectedNodeId'
                          }
                        , performBlind (Dom.focus selectedNodeIdHtmlAttribute)
                        )
                else
                    ( model, Cmd.none )

            ResetToSampleTree ->
                let
                    treeUndoList' =
                        MultiwayTreeZipper.goToChild 0 (initialZipper NoteMiner.SampleData.sampleTree)
                            |> justOrCrash "ResetToSampleTree"
                            |> getTreeRootFromZipper
                            |> \tree -> UndoList.fresh tree
                in
                    ( { model | treeUndoList = treeUndoList' }
                    , performBlind (Dom.focus selectedNodeIdHtmlAttribute)
                    )

            Undo ->
                let
                    treeUndoList' =
                        UndoList.undo model.treeUndoList
                in
                    ( { model | treeUndoList = treeUndoList' }
                    , performBlind (Dom.focus selectedNodeIdHtmlAttribute)
                    )

            Redo ->
                let
                    treeUndoList' =
                        UndoList.redo model.treeUndoList
                in
                    ( { model | treeUndoList = treeUndoList' }
                    , performBlind (Dom.focus selectedNodeIdHtmlAttribute)
                    )

            SetSearchText text ->
                ( { model | searchText = text }, Cmd.none )
