module NoteMiner exposing (..)

import Debug
import Dom
import Html.App as App
import MultiwayTreeZipper
import UndoList exposing (UndoList)
import NoteMiner.Constants exposing (selectedNodeIdHtmlAttribute)
import NoteMiner.SerializedTree exposing (SerializedTree)
import NoteMiner.Model exposing (Model)
import NoteMiner.SampleData exposing (sampleTree)
import NoteMiner.Update exposing (Msg, performBlind)
import NoteMiner.View exposing (view)
import NoteMiner.Storage as Storage
import NoteMiner.Tree exposing (initialZipper)
import NoteMiner.Maybe exposing (justOrCrash)


-- MAIN


main : Program (Maybe SerializedTree)
main =
    App.programWithFlags
        { init = init
        , view = view
        , update = Storage.update
        , subscriptions = always Sub.none
        }


init : Maybe SerializedTree -> ( Model, Cmd Msg )
init serializedTree =
    let
        tree =
            case serializedTree of
                Just tree ->
                    (case Storage.unserialize tree of
                        Ok tree ->
                            tree

                        Err msg ->
                            Debug.log
                                ("Error loading tree in localStorage: " ++ msg ++ ", fallback to sample tree")
                                sampleTree
                    )

                Nothing ->
                    Debug.log
                        "No tree found in localStorage, fallback to sample tree"
                        sampleTree

        firstChildZipper =
            MultiwayTreeZipper.goToChild 0 (initialZipper tree)
                |> justOrCrash "init"
    in
        ( { treeUndoList = UndoList.fresh tree
          , selectedNodeId = (MultiwayTreeZipper.datum firstChildZipper).id
          , isAltDown = False
          , isCtrlDown = False
          , isShiftDown = False
          , searchText = ""
          }
        , performBlind (Dom.focus selectedNodeIdHtmlAttribute)
        )
