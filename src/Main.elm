port module Main exposing (..)

import Debug
import Dom
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (id, style, value)
import Html.Events exposing (onClick, onInput)
import MultiwayTree
import MultiwayTreeZipper
import Task exposing (Task)
import Keyboard
import Json.Encode as Encode
import Json.Decode as Decode exposing ((:=))


-- MAIN


main : Program (Maybe SerializedTree)
main =
    App.programWithFlags
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = subscriptions
        }



-- CONSTANTS


up : Keyboard.KeyCode
up =
    38


down : Keyboard.KeyCode
down =
    40



-- HELPERS


justOrCrash : Maybe a -> String -> a
justOrCrash maybe msg =
    case maybe of
        Just value ->
            value

        Nothing ->
            Debug.crash msg


getNodeIdAttribute : Int -> String
getNodeIdAttribute id =
    ("node-" ++ (toString id))



-- TYPES


type alias Datum =
    { text : String, id : Int }


type alias Tree =
    MultiwayTree.Tree Datum


type alias Forest =
    MultiwayTree.Forest Datum


type alias Zipper =
    MultiwayTreeZipper.Zipper Datum


initialZipper : Tree -> Zipper
initialZipper tree =
    ( tree, [] )


goToBeginning : Tree -> Zipper
goToBeginning tree =
    justOrCrash
        (MultiwayTreeZipper.goToChild 0 (initialZipper tree))
        "`goToChild 0` should never return Nothing"


focusNode : Zipper -> Task Dom.Error ()
focusNode node =
    Dom.focus (getNodeIdAttribute (MultiwayTreeZipper.datum node).id)



-- SAMPLE DATA


node : String -> Int -> Forest -> Tree
node text id children =
    MultiwayTree.Tree { text = text, id = id } children


sampleTree : Tree
sampleTree =
    node "ROOT"
        0
        [ node "Inbox"
            1
            [ node "Faire les courses" 2 []
            , node "Aller au cinéma" 3 []
            , node "Rappeler ces personnes"
                4
                [ node "Bruno" 5 []
                , node "Laëtitia" 6 []
                ]
            ]
        , node "Projets"
            7
            [ node "NoteMinder" 8 []
            ]
        ]



-- MODEL


type alias Model =
    { tree : Tree
    , currentNode : Zipper
    }



-- INIT


init : Maybe SerializedTree -> ( Model, Cmd Msg )
init serializedTree =
    let
        tree =
            case serializedTree of
                Just tree ->
                    (case unserialize tree of
                        Ok tree ->
                            tree

                        Err msg ->
                            Debug.log ("Error loading tree in localStorage: " ++ msg) sampleTree
                    )

                Nothing ->
                    sampleTree

        currentNode =
            goToBeginning tree
    in
        ( { tree = tree
          , currentNode = currentNode
          }
        , Task.perform (\_ -> NoOp) (\_ -> NoOp) (focusNode currentNode)
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs KeyDown



-- UPDATE


type Msg
    = NoOp
    | SetCurrentNode Zipper
    | KeyDown Keyboard.KeyCode
    | UpdateNodeText String
    | ResetToSampleTree


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetCurrentNode zipper ->
            ( { model | currentNode = zipper }, Cmd.none )

        KeyDown keyCode ->
            if keyCode == up then
                let
                    currentNode' =
                        if model.currentNode == goToBeginning model.tree then
                            model.currentNode
                        else
                            case MultiwayTreeZipper.goToPrevious model.currentNode of
                                Just zipper ->
                                    zipper

                                Nothing ->
                                    model.currentNode
                in
                    ( { model | currentNode = currentNode' }
                    , Task.perform (\_ -> NoOp) (\_ -> NoOp) (focusNode currentNode')
                    )
            else if keyCode == down then
                let
                    currentNode' =
                        case MultiwayTreeZipper.goToNext model.currentNode of
                            Just zipper ->
                                zipper

                            Nothing ->
                                model.currentNode
                in
                    ( { model | currentNode = currentNode' }
                    , Task.perform (\_ -> NoOp) (\_ -> NoOp) (focusNode currentNode')
                    )
            else
                ( model, Cmd.none )

        UpdateNodeText text ->
            let
                currentNode' =
                    justOrCrash
                        (MultiwayTreeZipper.updateDatum (\datum -> { datum | text = text }) model.currentNode)
                        "`model.currentNode` always references an existing node"

                tree' =
                    fst
                        (justOrCrash
                            (MultiwayTreeZipper.goToRoot currentNode')
                            "goToRoot should never return Nothing"
                        )
            in
                ( { model | tree = tree', currentNode = currentNode' }, Cmd.none )

        ResetToSampleTree ->
            ( { model | tree = sampleTree, currentNode = goToBeginning sampleTree }, Cmd.none )



-- VIEWS


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "NoteMinder" ]
        , viewTree model.tree model.currentNode
        , hr [] []
        , button
            [ onClick ResetToSampleTree
            ]
            [ text "Reset to sample tree" ]
        ]


viewTree : Tree -> Zipper -> Html Msg
viewTree tree currentNode =
    viewForest (MultiwayTree.children tree) (initialZipper tree) currentNode


viewTreeNode : Tree -> Zipper -> Zipper -> List (Html Msg)
viewTreeNode node zipper currentNode =
    let
        datum =
            MultiwayTree.datum node

        liHtml =
            li
                [ onClick (SetCurrentNode zipper)
                ]
                [ input
                    [ id (getNodeIdAttribute datum.id)
                    , value datum.text
                    , onInput UpdateNodeText
                    , style
                        [ ( "border", "none" )
                        , ( "width", "100%" )
                        ]
                    ]
                    []
                ]

        nodeChildren =
            MultiwayTree.children node
    in
        if List.isEmpty nodeChildren then
            [ liHtml ]
        else
            [ liHtml, viewForest nodeChildren zipper currentNode ]


viewForest : Forest -> Zipper -> Zipper -> Html Msg
viewForest forest zipper currentNode =
    ul []
        (List.indexedMap
            (\index node ->
                let
                    zipper' =
                        justOrCrash
                            (MultiwayTreeZipper.goToChild index zipper)
                            "Should never reach this case (List.indexedMap reaches only existing children)"
                in
                    viewTreeNode node zipper' currentNode
            )
            forest
            |> List.concat
        )



-- STORAGE


type alias SerializedTree =
    String


port setStorage : SerializedTree -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ setStorage (serialize model.tree), cmds ]
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
    Decode.object2
        DatumPair
        ("datum" := decodeDatum)
        ("children" := lazy (\_ -> Decode.list decodeTreeNode))


decodeDatum : Decode.Decoder Datum
decodeDatum =
    Decode.object2
        Datum
        ("text" := Decode.string)
        ("id" := Decode.int)


lazy : (() -> Decode.Decoder a) -> Decode.Decoder a
lazy thunk =
    Decode.customDecoder
        Decode.value
        (\rawValue -> Decode.decodeValue (thunk ()) rawValue)
