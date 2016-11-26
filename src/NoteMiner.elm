module NoteMiner exposing (..)

import Debug
import Dom
import Html
import Json.Decode as Decode
import MultiwayTreeZipper
import UndoList exposing (UndoList)
import NoteMiner.Constants exposing (selectedNodeIdHtmlAttribute)
import NoteMiner.Model exposing (Model)
import NoteMiner.SampleData exposing (sampleTree)
import NoteMiner.Update exposing (Msg(..))
import NoteMiner.View exposing (view)
import NoteMiner.Storage as Storage
import NoteMiner.Tree exposing (initialZipper)
import NoteMiner.Maybe exposing (justOrCrash)
import Task


-- MAIN


main : Program Decode.Value Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = Storage.update
        , subscriptions = always Sub.none
        }


init : Decode.Value -> ( Model, Cmd Msg )
init serializedTree =
    case Decode.decodeValue (Decode.maybe Decode.string) serializedTree of
        Ok serializedTree ->
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
                , Task.attempt (\_ -> NoteMiner.Update.NoOp) (Dom.focus selectedNodeIdHtmlAttribute)
                )

        Err err ->
            Debug.crash err
