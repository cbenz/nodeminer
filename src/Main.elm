port module Main exposing (..)

import Char
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


shift : KeyCode
shift =
    16


ctrl : KeyCode
ctrl =
    17


up : KeyCode
up =
    38


down : KeyCode
down =
    40



-- LIST HELPERS


{-| Inserts a value in a list so that the value has the given index in the new list.

    insertAtIndex 9 1 [0,1,2]
        [0,9,1,2]
-}
insertAtIndex : a -> Int -> List a -> List a
insertAtIndex x index xs =
    let
        startSlice =
            List.take index xs

        endSlice =
            List.drop index xs
    in
        startSlice ++ x :: endSlice


removeAtIndex : Int -> List a -> List a
removeAtIndex index xs =
    let
        startSlice =
            List.take index xs

        endSlice =
            List.drop (index + 1) xs
    in
        startSlice ++ endSlice



-- MAYBE HELPERS


justOrCrash : String -> Maybe a -> a
justOrCrash msg maybe =
    case maybe of
        Just value ->
            value

        Nothing ->
            Debug.crash (msg ++ ": a value should never have been `Nothing`.")



-- HTML HELPERS


getNodeIdAttribute : Int -> String
getNodeIdAttribute id =
    "node-" ++ (toString id)



-- TASK HELPERS


focusDomNode : Zipper -> Cmd Msg
focusDomNode node =
    Task.perform
        (\_ -> NoOp)
        (\_ -> NoOp)
        (Dom.focus (getNodeIdAttribute (MultiwayTreeZipper.datum node).id))



-- TYPES


type alias Datum =
    { text : String, id : Int }


type alias Tree =
    MultiwayTree.Tree Datum


type alias Forest =
    MultiwayTree.Forest Datum


type alias Zipper =
    MultiwayTreeZipper.Zipper Datum


node : String -> Int -> Forest -> Tree
node text id children =
    MultiwayTree.Tree { text = text, id = id } children


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


nextNode : Zipper -> Tree
nextNode zipper =
    let
        tree =
            getTreeRootFromZipper zipper
    in
        node "" (nextId tree) []


hasSameDatumThan : MultiwayTree.Tree a -> MultiwayTree.Tree a -> Bool
hasSameDatumThan a b =
    MultiwayTree.datum a == MultiwayTree.datum b


initialZipper : MultiwayTree.Tree a -> MultiwayTreeZipper.Zipper a
initialZipper tree =
    ( tree, [] )


isFirstVisibleNode : MultiwayTreeZipper.Zipper a -> Bool
isFirstVisibleNode zipper =
    let
        firstVisibleNode =
            MultiwayTreeZipper.goToRoot zipper
                `Maybe.andThen` (MultiwayTreeZipper.goToChild 0)
                |> justOrCrash "isFirstVisibleNode"
    in
        zipper == firstVisibleNode


isFirstLevelNode : MultiwayTreeZipper.Zipper a -> Bool
isFirstLevelNode zipper =
    Maybe.map2 (\parent root -> (fst parent) `hasSameDatumThan` (fst root))
        (MultiwayTreeZipper.goUp zipper)
        (MultiwayTreeZipper.goToRoot zipper)
        |> justOrCrash "isFirstLevelNode"


hasChildren : MultiwayTreeZipper.Zipper a -> Bool
hasChildren zipper =
    zipper |> fst |> MultiwayTree.children |> not << List.isEmpty


getTreeRootFromZipper : MultiwayTreeZipper.Zipper a -> MultiwayTree.Tree a
getTreeRootFromZipper zipper =
    MultiwayTreeZipper.goToRoot zipper
        |> justOrCrash "getTreeRootFromZipper"
        |> fst


getSiblings : MultiwayTreeZipper.Zipper a -> MultiwayTree.Forest a
getSiblings zipper =
    let
        parent =
            MultiwayTreeZipper.goUp zipper
                |> justOrCrash "getSiblings"
    in
        MultiwayTree.children (fst parent)


findIndexInForest : MultiwayTreeZipper.Zipper a -> MultiwayTree.Forest a -> Int
findIndexInForest zipper forest =
    let
        node =
            fst zipper
    in
        forest
            |> List.indexedMap
                (\index sibling ->
                    if sibling `hasSameDatumThan` node then
                        Just index
                    else
                        Nothing
                )
            |> Maybe.oneOf
            |> justOrCrash "findIndexInForest"


findIndexInSiblings : MultiwayTreeZipper.Zipper a -> Int
findIndexInSiblings zipper =
    let
        siblings =
            getSiblings zipper
    in
        findIndexInForest zipper siblings


isLastSibling : MultiwayTreeZipper.Zipper a -> Bool
isLastSibling zipper =
    let
        nbSiblings =
            getSiblings zipper |> List.length

        index =
            findIndexInSiblings zipper
    in
        index == nbSiblings - 1


{-| Inserts a `Tree` as the next sibling of the current focus. Does not move the focus.
-}
insertSiblingBelow : MultiwayTree.Tree a -> MultiwayTreeZipper.Zipper a -> Maybe (MultiwayTreeZipper.Zipper a)
insertSiblingBelow newTree zipper =
    let
        index =
            findIndexInSiblings zipper
    in
        insertSiblingAtIndex newTree (index + 1) zipper


{-| Inserts a `Tree` as the child of `index` of the `Tree` of the current focus. Does not move the focus.
-}
insertChildAtIndex : MultiwayTree.Tree a -> Int -> MultiwayTreeZipper.Zipper a -> Maybe (MultiwayTreeZipper.Zipper a)
insertChildAtIndex newTree index zipper =
    let
        children =
            MultiwayTree.children (fst zipper)

        children' =
            insertAtIndex newTree index children
    in
        MultiwayTreeZipper.updateChildren children' zipper


{-| Inserts a `Tree` as the sibling of `index` of the `Tree` of the current focus. Does not move the focus.
-}
insertSiblingAtIndex : MultiwayTree.Tree a -> Int -> MultiwayTreeZipper.Zipper a -> Maybe (MultiwayTreeZipper.Zipper a)
insertSiblingAtIndex newTree index zipper =
    MultiwayTreeZipper.goUp zipper
        `Maybe.andThen` insertChildAtIndex newTree index
        `Maybe.andThen`
            -- Restore focus moved by `goUp` to its original position.
            MultiwayTreeZipper.goToChild (index - 1)


{-| Removes the `Tree` at the current focus. Moves the focus to the parent.
-}
removeCurrentAndGoUp : MultiwayTreeZipper.Zipper a -> Maybe (MultiwayTreeZipper.Zipper a)
removeCurrentAndGoUp zipper =
    let
        index =
            findIndexInSiblings zipper

        siblings =
            getSiblings zipper

        children' =
            removeAtIndex index siblings
    in
        MultiwayTreeZipper.goUp zipper
            `Maybe.andThen` MultiwayTreeZipper.updateChildren children'


goToCorrectChildAfterRemoval :
    MultiwayTreeZipper.Zipper a
    -> MultiwayTreeZipper.Zipper a
    -> Maybe (MultiwayTreeZipper.Zipper a)
goToCorrectChildAfterRemoval zipper parent =
    if hasChildren parent then
        if isLastSibling zipper then
            MultiwayTreeZipper.goToRightMostChild parent
        else
            let
                index =
                    findIndexInSiblings zipper
            in
                MultiwayTreeZipper.goToChild index parent
    else
        Just parent


goToNextSibling : MultiwayTreeZipper.Zipper a -> Maybe (MultiwayTreeZipper.Zipper a)
goToNextSibling zipper =
    let
        index =
            findIndexInSiblings zipper
    in
        MultiwayTreeZipper.goUp zipper
            `Maybe.andThen` MultiwayTreeZipper.goToChild (index + 1)



-- MultiwayTreeZipper.goToChild index parent


removeCurrent : MultiwayTreeZipper.Zipper a -> Maybe (MultiwayTreeZipper.Zipper a)
removeCurrent zipper =
    removeCurrentAndGoUp zipper
        `Maybe.andThen` goToCorrectChildAfterRemoval zipper



-- SAMPLE DATA


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
    { currentNode : Zipper
    , isCtrlPressed : Bool
    , isShiftPressed : Bool
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
                            Debug.log
                                ("Error loading tree in localStorage: " ++ msg ++ ", fallback to sample tree")
                                sampleTree
                    )

                Nothing ->
                    Debug.log
                        "No tree found in localStorage, fallback to sample tree"
                        sampleTree

        currentNode =
            case MultiwayTreeZipper.goToChild 0 (initialZipper tree) of
                Just currentNode ->
                    currentNode

                Nothing ->
                    Debug.log
                        "Error with `goToChild 0` on loaded tree from localStorage, fallback to sample tree"
                        (initialZipper sampleTree)
    in
        ( { currentNode = currentNode
          , isCtrlPressed = False
          , isShiftPressed = False
          }
        , focusDomNode currentNode
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]



-- UPDATE


type Msg
    = NoOp
    | SetCurrentNode Zipper
    | KeyDown KeyCode
    | KeyUp KeyCode
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
            if keyCode == ctrl then
                ( { model | isCtrlPressed = True }, Cmd.none )
            else if keyCode == shift then
                ( { model | isShiftPressed = True }, Cmd.none )
            else if keyCode == up then
                let
                    currentNode' =
                        if isFirstVisibleNode model.currentNode then
                            model.currentNode
                        else
                            MultiwayTreeZipper.goToPrevious model.currentNode
                                |> justOrCrash "update: keyCode == up"
                in
                    ( { model | currentNode = currentNode' }
                    , focusDomNode currentNode'
                    )
            else if keyCode == down then
                let
                    currentNode' =
                        MultiwayTreeZipper.goToNext model.currentNode
                            |> Maybe.withDefault model.currentNode
                in
                    ( { model | currentNode = currentNode' }
                    , focusDomNode currentNode'
                    )
            else if keyCode == enter then
                let
                    newNode =
                        nextNode model.currentNode

                    currentNode' =
                        if hasChildren model.currentNode then
                            MultiwayTreeZipper.insertChild newNode model.currentNode
                                `Maybe.andThen` MultiwayTreeZipper.goToChild 0
                                |> justOrCrash "update: keyCode == enter; goToChild 0"
                        else
                            insertSiblingBelow newNode model.currentNode
                                `Maybe.andThen` MultiwayTreeZipper.goToNext
                                |> justOrCrash "update: keyCode == enter; goToNext"
                in
                    ( { model | currentNode = currentNode' }
                    , focusDomNode currentNode'
                    )
            else if keyCode == tab then
                if model.isShiftPressed then
                    if isFirstLevelNode model.currentNode then
                        -- To be dedented the node must have a depth > 1.
                        ( model, Cmd.none )
                    else
                        let
                            currentNode' =
                                removeCurrentAndGoUp model.currentNode
                                    `Maybe.andThen` insertSiblingBelow (fst model.currentNode)
                                    `Maybe.andThen` goToNextSibling
                                    |> justOrCrash "update: keyCode == tab; model.isShiftPressed"
                        in
                            ( { model | currentNode = currentNode' }
                            , focusDomNode currentNode'
                            )
                else
                    let
                        index =
                            findIndexInSiblings model.currentNode
                    in
                        if index > 0 then
                            let
                                currentNode' =
                                    removeCurrentAndGoUp model.currentNode
                                        `Maybe.andThen` MultiwayTreeZipper.goToChild (index - 1)
                                        `Maybe.andThen` MultiwayTreeZipper.appendChild (fst model.currentNode)
                                        `Maybe.andThen` MultiwayTreeZipper.goToRightMostChild
                                        |> justOrCrash "update: keyCode == tab; not model.isShiftPressed; index > 0"
                            in
                                ( { model | currentNode = currentNode' }
                                , focusDomNode currentNode'
                                )
                        else
                            -- To be indented the node must have at least a previous sibling.
                            ( model, Cmd.none )
            else if Char.fromCode keyCode == 'K' then
                if isFirstVisibleNode model.currentNode && List.length (getSiblings model.currentNode) == 1 then
                    -- Cannot remove latest first-level node.
                    ( model, Cmd.none )
                else
                    let
                        currentNode' =
                            removeCurrent model.currentNode
                                |> justOrCrash "update: keyCode == K"
                    in
                        ( { model | currentNode = currentNode' }
                        , focusDomNode currentNode'
                        )
            else
                -- let
                --     _ =
                --         Debug.log "unknown keyCode" keyCode
                -- in
                ( model, Cmd.none )

        KeyUp keyCode ->
            if keyCode == ctrl then
                ( { model | isCtrlPressed = False }, Cmd.none )
            else if keyCode == shift then
                ( { model | isShiftPressed = False }, Cmd.none )
            else
                ( model, Cmd.none )

        UpdateNodeText text ->
            let
                currentNode' =
                    MultiwayTreeZipper.updateDatum
                        (\datum -> { datum | text = text })
                        model.currentNode
                        |> justOrCrash "update: UpdateNodeText"
            in
                ( { model | currentNode = currentNode' }
                , Cmd.none
                )

        ResetToSampleTree ->
            let
                currentNode =
                    MultiwayTreeZipper.goToChild 0 (initialZipper sampleTree)
                        |> justOrCrash "update: ResetToSampleTree"
            in
                ( { model | currentNode = currentNode }
                , Cmd.none
                )



-- VIEWS


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "NoteMiner" ]
        , viewTree model.currentNode
        , hr [] []
        , p []
            [ text "Keyboard shortcuts"
            , ul []
                [ li [] [ text "Up - select previous node" ]
                , li [] [ text "Down - select next node" ]
                , li [] [ text "Enter - insert a node below" ]
                , li [] [ text "Tab - indent a node" ]
                , li [] [ text "Shift-Tab - dedent a node" ]
                , li [] [ text "Ctrl-Shift-K - remove the current node" ]
                ]
            ]
        , button
            [ onClick ResetToSampleTree
            ]
            [ text "Reset to sample tree" ]
        , p []
            [ "The tree is stored and synchronized in localStorage. "
                ++ "Look at the JavaScript console for error messages."
                |> text
            ]
        ]


viewTree : Zipper -> Html Msg
viewTree currentNode =
    let
        tree =
            getTreeRootFromZipper currentNode
    in
        viewForest
            (MultiwayTree.children tree)
            (initialZipper tree)
            currentNode


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
                    , preventDefaultForKeyCodes [ enter, up, down, tab ]
                    , style
                        (let
                            isCurrentNode =
                                (fst zipper) `hasSameDatumThan` (fst currentNode)

                            highlightedStyle =
                                if isCurrentNode then
                                    [ ( "background-color", "lightblue" ) ]
                                else
                                    []
                         in
                            [ ( "border", "none" )
                            , ( "width", "100%" )
                            ]
                                ++ highlightedStyle
                        )
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
                        MultiwayTreeZipper.goToChild index zipper
                            |> justOrCrash "viewForest"
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

        tree =
            getTreeRootFromZipper model.currentNode
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
