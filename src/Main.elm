port module Main exposing (..)

import Debug
import Dom
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (id, style, value)
import Html.Events exposing (onClick, onInput, onWithOptions)
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


type alias KeyCode =
    Int


tab : KeyCode
tab =
    9


enter : KeyCode
enter =
    13


up : KeyCode
up =
    38


down : KeyCode
down =
    40



-- HELPERS


justOrCrash : String -> Maybe a -> a
justOrCrash msg maybe =
    case maybe of
        Just value ->
            value

        Nothing ->
            Debug.crash msg


getNodeIdAttribute : Int -> String
getNodeIdAttribute id =
    "node-" ++ (toString id)



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
        "`goToChild 0` should never return Nothing"
        (MultiwayTreeZipper.goToChild 0 (initialZipper tree))


focusNode : Zipper -> Task Dom.Error ()
focusNode node =
    Dom.focus (getNodeIdAttribute (MultiwayTreeZipper.datum node).id)


getTreeRootFromZipper : Zipper -> Tree
getTreeRootFromZipper zipper =
    fst
        (justOrCrash
            "goToRoot should never return Nothing"
            (MultiwayTreeZipper.goToRoot zipper)
        )


findIndex : Tree -> Forest -> Int
findIndex node nodes =
    nodes
        |> List.indexedMap
            (\index child ->
                if MultiwayTree.datum child == MultiwayTree.datum node then
                    Just index
                else
                    Nothing
            )
        |> Maybe.oneOf
        |> justOrCrash "find a child from a parent should never return Nothing"


insertAtIndex : Tree -> Int -> Forest -> Forest
insertAtIndex node index nodes =
    let
        startSlice =
            List.take index nodes

        endSlice =
            List.drop index nodes
    in
        startSlice ++ node :: endSlice


nextId : Tree -> Int
nextId tree =
    tree
        |> MultiwayTree.flatten
        |> List.map .id
        |> List.maximum
        |> (\index ->
                case index of
                    Just index ->
                        index + 1

                    Nothing ->
                        0
           )


createNode : Tree -> Tree
createNode tree =
    node "" (nextId tree) []


insertSiblingAfter : Tree -> Zipper -> Tree -> Maybe Zipper
insertSiblingAfter newTree zipper tree =
    let
        parent =
            (justOrCrash
                "goUp should never return Nothing because root node is unreachable by the user"
                (MultiwayTreeZipper.goUp zipper)
            )

        children =
            MultiwayTree.children (fst parent)

        index =
            findIndex (fst zipper) children

        children' =
            insertAtIndex newTree (index + 1) children

        parent' =
            MultiwayTreeZipper.updateChildren children' parent
    in
        parent' `Maybe.andThen` MultiwayTreeZipper.goToChild index



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
            [ node "NoteMiner" 8 []
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
    | KeyDown KeyCode
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
            else if keyCode == enter then
                let
                    newNode =
                        createNode model.tree

                    currentNode' =
                        justOrCrash
                            "insertion should never fail"
                            (if List.isEmpty (MultiwayTree.children (fst model.currentNode)) then
                                insertSiblingAfter newNode model.currentNode model.tree
                                    `Maybe.andThen` MultiwayTreeZipper.goToNext
                             else
                                MultiwayTreeZipper.insertChild newNode model.currentNode
                                    `Maybe.andThen` MultiwayTreeZipper.goToChild 0
                            )

                    tree' =
                        getTreeRootFromZipper currentNode'
                in
                    ( { model | tree = tree', currentNode = currentNode' }
                    , Task.perform (\_ -> NoOp) (\_ -> NoOp) (focusNode currentNode')
                    )
                -- else if keyCode == tab then
                --     ( model, Cmd.none )
            else
                ( model, Cmd.none )

        UpdateNodeText text ->
            let
                currentNode' =
                    justOrCrash
                        "`model.currentNode` always references an existing node"
                        (MultiwayTreeZipper.updateDatum
                            (\datum -> { datum | text = text })
                            model.currentNode
                        )

                tree' =
                    getTreeRootFromZipper currentNode'
            in
                ( { model | tree = tree', currentNode = currentNode' }, Cmd.none )

        ResetToSampleTree ->
            ( { model | tree = sampleTree, currentNode = goToBeginning sampleTree }, Cmd.none )



-- VIEWS


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "NoteMiner" ]
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

        preventDefaultForKeyCodes : List KeyCode -> Attribute Msg
        preventDefaultForKeyCodes keyCodes =
            let
                eventOptions =
                    { stopPropagation = False, preventDefault = True }

                filterKey keyCode =
                    if List.any ((==) keyCode) keyCodes then
                        Ok keyCode
                    else
                        Err "ignored input"

                decoder =
                    Decode.customDecoder Html.Events.keyCode filterKey
                        |> Decode.map (always NoOp)
            in
                onWithOptions "keydown" eventOptions decoder

        liHtml =
            li
                [ onClick (SetCurrentNode zipper)
                ]
                [ input
                    [ id (getNodeIdAttribute datum.id)
                    , value datum.text
                    , onInput UpdateNodeText
                    , preventDefaultForKeyCodes [ enter, up, down ]
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
                            "Should never reach this case (List.indexedMap reaches only existing children)"
                            (MultiwayTreeZipper.goToChild index zipper)
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
