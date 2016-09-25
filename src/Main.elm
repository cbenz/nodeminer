port module Main exposing (..)

import Char
import String
import Regex exposing (Regex)
import Debug
import Dom
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (disabled, href, id, rel, style, target, title, value)
import Html.Events exposing (on, onClick, onInput, onWithOptions)
import MultiwayTree
import MultiwayTreeZipper
import Task exposing (Task)
import Json.Encode as Encode
import Json.Decode as Decode exposing ((:=))
import UndoList exposing (UndoList)


-- MAIN


main : Program (Maybe SerializedTree)
main =
    App.programWithFlags
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = always Sub.none
        }



-- CONSTANTS


type alias KeyCode =
    Int


backspace : KeyCode
backspace =
    8


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


alt : KeyCode
alt =
    18


up : KeyCode
up =
    38


down : KeyCode
down =
    40


selectedNodeId : String
selectedNodeId =
    "selected-node"



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



-- TASK HELPERS


performBlind : Task a b -> Cmd Msg
performBlind =
    Task.perform (always NoOp) (always NoOp)



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


getText : Zipper -> String
getText =
    fst >> MultiwayTree.datum >> .text


isTextEmpty : Zipper -> Bool
isTextEmpty =
    getText >> String.isEmpty


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


isFirstSibling : MultiwayTreeZipper.Zipper a -> Bool
isFirstSibling zipper =
    let
        index =
            findIndexInSiblings zipper
    in
        index == 0


isLastSibling : MultiwayTreeZipper.Zipper a -> Bool
isLastSibling zipper =
    let
        nbSiblings =
            getSiblings zipper |> List.length

        index =
            findIndexInSiblings zipper
    in
        index == nbSiblings - 1


canGoToPrevious : MultiwayTreeZipper.Zipper a -> Bool
canGoToPrevious =
    not << isFirstVisibleNode


canGoToNext : MultiwayTreeZipper.Zipper a -> Bool
canGoToNext zipper =
    case MultiwayTreeZipper.goToNext zipper of
        Just _ ->
            True

        Nothing ->
            False


{-| Cannot remove latest first-level node.
-}
canRemove : MultiwayTreeZipper.Zipper a -> Bool
canRemove zipper =
    not (isFirstVisibleNode zipper && List.length (getSiblings zipper) == 1)


canIndent : MultiwayTreeZipper.Zipper a -> Bool
canIndent zipper =
    findIndexInSiblings zipper > 0


canDedent : MultiwayTreeZipper.Zipper a -> Bool
canDedent =
    not << isFirstLevelNode


canMoveUp : MultiwayTreeZipper.Zipper a -> Bool
canMoveUp =
    not << isFirstSibling


canMoveDown : MultiwayTreeZipper.Zipper a -> Bool
canMoveDown =
    not << isLastSibling


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


goToNextSibling : MultiwayTreeZipper.Zipper a -> Maybe (MultiwayTreeZipper.Zipper a)
goToNextSibling zipper =
    let
        index =
            findIndexInSiblings zipper
    in
        MultiwayTreeZipper.goUp zipper
            `Maybe.andThen` MultiwayTreeZipper.goToChild (index + 1)



-- SAMPLE DATA


sampleTree : Tree
sampleTree =
    node "ROOT"
        0
        [ node "Inbox"
            1
            [ node "Faire les courses #urgent" 2 []
            , node "Aller au cinéma http://allocine.com/" 3 []
            , node "Rappeler ces personnes"
                4
                [ node "@Bruno" 5 []
                , node "@Laëtitia" 6 []
                ]
            ]
        , node "Projets"
            7
            [ node "NoteMiner" 8 []
            ]
        ]



-- MODEL


type alias Model =
    { currentZipperUndoList : UndoList Zipper
    , isAltDown : Bool
    , isCtrlDown : Bool
    , isShiftDown : Bool
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

        zipper =
            case MultiwayTreeZipper.goToChild 0 (initialZipper tree) of
                Just zipper ->
                    zipper

                Nothing ->
                    Debug.log
                        "Error with `goToChild 0` on loaded tree from localStorage, fallback to sample tree"
                        (initialZipper sampleTree)
    in
        ( { currentZipperUndoList = UndoList.fresh zipper
          , isAltDown = False
          , isCtrlDown = False
          , isShiftDown = False
          }
        , performBlind (Dom.focus selectedNodeId)
        )



-- UPDATE


type Msg
    = NoOp
    | FocusNode Zipper
    | KeyDown KeyCode
    | KeyUp KeyCode
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
    | ResetToSampleTree
    | Undo
    | Redo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        currentZipper =
            model.currentZipperUndoList.present

        updateMoveCurrentNodeUp () =
            if canMoveUp currentZipper then
                let
                    index =
                        findIndexInSiblings currentZipper

                    index' =
                        index - 1

                    currentZipperUndoList' =
                        removeCurrentAndGoUp currentZipper
                            `Maybe.andThen` insertChildAtIndex (fst currentZipper) index'
                            `Maybe.andThen` MultiwayTreeZipper.goToChild index'
                            |> justOrCrash "MoveCurrentNodeUp"
                            |> (\state -> UndoList.new state model.currentZipperUndoList)
                in
                    ( { model | currentZipperUndoList = currentZipperUndoList' }
                    , performBlind (Dom.focus selectedNodeId)
                    )
            else
                ( model, Cmd.none )

        updateMoveCurrentNodeDown () =
            if canMoveDown currentZipper then
                let
                    index =
                        findIndexInSiblings currentZipper

                    index' =
                        index + 1

                    currentZipperUndoList' =
                        removeCurrentAndGoUp currentZipper
                            `Maybe.andThen` insertChildAtIndex (fst currentZipper) index'
                            `Maybe.andThen` MultiwayTreeZipper.goToChild index'
                            |> justOrCrash "MoveCurrentNodeDown"
                            |> (\state -> UndoList.new state model.currentZipperUndoList)
                in
                    ( { model | currentZipperUndoList = currentZipperUndoList' }
                    , performBlind (Dom.focus selectedNodeId)
                    )
            else
                ( model, Cmd.none )

        updateSelectPreviousNode () =
            let
                currentZipperUndoList' =
                    UndoList.mapPresent
                        (\zipper ->
                            if canGoToPrevious zipper then
                                MultiwayTreeZipper.goToPrevious zipper
                                    |> justOrCrash "SelectPreviousNode"
                            else
                                zipper
                        )
                        model.currentZipperUndoList
            in
                ( { model | currentZipperUndoList = currentZipperUndoList' }
                , performBlind (Dom.focus selectedNodeId)
                )

        updateSelectNextNode () =
            let
                currentZipperUndoList' =
                    UndoList.mapPresent
                        (\zipper ->
                            if canGoToNext zipper then
                                MultiwayTreeZipper.goToNext zipper
                                    |> justOrCrash "SelectNextNode"
                            else
                                zipper
                        )
                        model.currentZipperUndoList
            in
                ( { model | currentZipperUndoList = currentZipperUndoList' }
                , performBlind (Dom.focus selectedNodeId)
                )

        updateInsertNodeBelow () =
            let
                newNode =
                    nextNode currentZipper

                currentZipperUndoList' =
                    (if hasChildren currentZipper then
                        MultiwayTreeZipper.insertChild newNode currentZipper
                            `Maybe.andThen` MultiwayTreeZipper.goToChild 0
                            |> justOrCrash "InsertNodeBelow; goToChild 0"
                     else
                        insertSiblingBelow newNode currentZipper
                            `Maybe.andThen` MultiwayTreeZipper.goToNext
                            |> justOrCrash "InsertNodeBelow; goToNext"
                    )
                        |> (\state -> UndoList.new state model.currentZipperUndoList)
            in
                ( { model | currentZipperUndoList = currentZipperUndoList' }
                , performBlind (Dom.focus selectedNodeId)
                )

        updateIndentCurrentNode () =
            let
                index =
                    findIndexInSiblings currentZipper
            in
                if canIndent currentZipper then
                    let
                        currentZipperUndoList' =
                            removeCurrentAndGoUp currentZipper
                                `Maybe.andThen` MultiwayTreeZipper.goToChild (index - 1)
                                `Maybe.andThen` MultiwayTreeZipper.appendChild (fst currentZipper)
                                `Maybe.andThen` MultiwayTreeZipper.goToRightMostChild
                                |> justOrCrash "IndentCurrentNode"
                                |> (\state -> UndoList.new state model.currentZipperUndoList)
                    in
                        ( { model | currentZipperUndoList = currentZipperUndoList' }
                        , performBlind (Dom.focus selectedNodeId)
                        )
                else
                    -- To be indented the node must have at least a previous sibling.
                    ( model, Cmd.none )

        updateDedentCurrentNode () =
            if canDedent currentZipper then
                let
                    currentZipperUndoList' =
                        removeCurrentAndGoUp currentZipper
                            `Maybe.andThen` insertSiblingBelow (fst currentZipper)
                            `Maybe.andThen` goToNextSibling
                            |> justOrCrash "DedentCurrentNode"
                            |> (\state -> UndoList.new state model.currentZipperUndoList)
                in
                    ( { model | currentZipperUndoList = currentZipperUndoList' }
                    , performBlind (Dom.focus selectedNodeId)
                    )
            else
                -- To be dedented the node must have a depth > 1.
                ( model, Cmd.none )

        updateRemoveCurrentNode () =
            if canRemove currentZipper then
                let
                    currentZipperUndoList' =
                        removeCurrentAndGoUp currentZipper
                            `Maybe.andThen`
                                (\parent ->
                                    if hasChildren parent then
                                        if isLastSibling currentZipper then
                                            MultiwayTreeZipper.goToRightMostChild parent
                                        else
                                            let
                                                index =
                                                    findIndexInSiblings currentZipper
                                            in
                                                MultiwayTreeZipper.goToChild index parent
                                    else
                                        Just parent
                                )
                            |> justOrCrash "RemoveCurrentNode"
                            |> (\state -> UndoList.new state model.currentZipperUndoList)
                in
                    ( { model | currentZipperUndoList = currentZipperUndoList' }
                    , performBlind (Dom.focus selectedNodeId)
                    )
            else
                ( model, Cmd.none )

        updateUndo () =
            let
                currentZipperUndoList' =
                    UndoList.undo model.currentZipperUndoList
            in
                ( { model | currentZipperUndoList = currentZipperUndoList' }
                , performBlind (Dom.focus selectedNodeId)
                )

        updateRedo () =
            let
                currentZipperUndoList' =
                    UndoList.redo model.currentZipperUndoList
            in
                ( { model | currentZipperUndoList = currentZipperUndoList' }
                , performBlind (Dom.focus selectedNodeId)
                )
    in
        case msg of
            NoOp ->
                ( model, Cmd.none )

            FocusNode zipper ->
                let
                    currentZipperUndoList' =
                        UndoList.mapPresent (always zipper) model.currentZipperUndoList
                in
                    ( { model | currentZipperUndoList = currentZipperUndoList' }
                    , performBlind (Dom.focus selectedNodeId)
                    )

            KeyDown keyCode ->
                if keyCode == alt then
                    ( { model | isAltDown = True }, Cmd.none )
                else if keyCode == ctrl then
                    ( { model | isCtrlDown = True }, Cmd.none )
                else if keyCode == shift then
                    ( { model | isShiftDown = True }, Cmd.none )
                else if keyCode == up then
                    if model.isAltDown then
                        updateMoveCurrentNodeUp ()
                    else
                        updateSelectPreviousNode ()
                else if keyCode == down then
                    if model.isAltDown then
                        updateMoveCurrentNodeDown ()
                    else
                        updateSelectNextNode ()
                else if keyCode == enter then
                    updateInsertNodeBelow ()
                else if keyCode == tab then
                    if model.isShiftDown then
                        updateDedentCurrentNode ()
                    else
                        updateIndentCurrentNode ()
                else if Char.fromCode keyCode == 'K' && model.isCtrlDown && model.isShiftDown then
                    updateRemoveCurrentNode ()
                else if Char.fromCode keyCode == 'Z' && model.isCtrlDown then
                    if model.isShiftDown then
                        updateRedo ()
                    else
                        updateUndo ()
                else
                    ( model, Cmd.none )

            KeyUp keyCode ->
                if keyCode == alt then
                    ( { model | isAltDown = False }, Cmd.none )
                else if keyCode == ctrl then
                    ( { model | isCtrlDown = False }, Cmd.none )
                else if keyCode == shift then
                    ( { model | isShiftDown = False }, Cmd.none )
                else
                    ( model, Cmd.none )

            SetText text ->
                let
                    setText zipper =
                        MultiwayTreeZipper.updateDatum
                            (\datum -> { datum | text = text })
                            zipper
                            |> justOrCrash "SetText"

                    currentZipperUndoList' =
                        if String.endsWith " " text then
                            UndoList.new (setText currentZipper) model.currentZipperUndoList
                        else
                            UndoList.mapPresent setText model.currentZipperUndoList
                in
                    ( { model | currentZipperUndoList = currentZipperUndoList' }
                    , Cmd.none
                    )

            SelectPreviousNode ->
                updateSelectPreviousNode ()

            SelectNextNode ->
                updateSelectNextNode ()

            InsertNodeBelow ->
                updateInsertNodeBelow ()

            IndentCurrentNode ->
                updateIndentCurrentNode ()

            DedentCurrentNode ->
                updateDedentCurrentNode ()

            MoveCurrentNodeUp ->
                updateMoveCurrentNodeUp ()

            MoveCurrentNodeDown ->
                updateMoveCurrentNodeDown ()

            RemoveCurrentNode ->
                updateRemoveCurrentNode ()

            RemoveCurrentNodeFromBackspace ->
                if canRemove currentZipper then
                    let
                        currentZipperUndoList' =
                            removeCurrentAndGoUp currentZipper
                                `Maybe.andThen`
                                    (\parent ->
                                        if hasChildren parent then
                                            let
                                                index =
                                                    findIndexInSiblings currentZipper
                                            in
                                                MultiwayTreeZipper.goToChild (max 0 (index - 1)) parent
                                        else
                                            Just parent
                                    )
                                |> justOrCrash "RemoveCurrentNodeFromBackspace"
                                |> (\state -> UndoList.new state model.currentZipperUndoList)
                    in
                        ( { model | currentZipperUndoList = currentZipperUndoList' }
                        , performBlind (Dom.focus selectedNodeId)
                        )
                else
                    ( model, Cmd.none )

            ResetToSampleTree ->
                let
                    currentZipperUndoList' =
                        MultiwayTreeZipper.goToChild 0 (initialZipper sampleTree)
                            |> justOrCrash "ResetToSampleTree"
                            |> (\state -> UndoList.new state model.currentZipperUndoList)
                in
                    ( { model | currentZipperUndoList = currentZipperUndoList' }
                    , performBlind (Dom.focus selectedNodeId)
                    )

            Undo ->
                updateUndo ()

            Redo ->
                updateRedo ()



-- VIEWS


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "NoteMiner" ]
        , viewToolbar model
        , viewTree model
        , hr [] []
        , button [ onClick ResetToSampleTree ] [ text "Reset to sample tree" ]
        , p []
            [ "The tree is stored and synchronized in localStorage. "
                ++ "Look at the JavaScript console for error messages."
                |> text
            ]
        ]


viewTree : Model -> Html Msg
viewTree model =
    let
        currentZipper =
            model.currentZipperUndoList.present

        tree =
            getTreeRootFromZipper currentZipper

        onKeyDown : (Int -> msg) -> Attribute msg
        onKeyDown tagger =
            on "keydown" (Decode.map tagger Html.Events.keyCode)

        onKeyUp : (Int -> msg) -> Attribute msg
        onKeyUp tagger =
            on "keyup" (Decode.map tagger Html.Events.keyCode)
    in
        div
            [ onKeyDown KeyDown
            , onKeyUp KeyUp
            , style [ ( "font-family", "sans-serif" ), ( "font-size", "1em" ) ]
            ]
            [ viewTreeNodeChildren model (initialZipper tree) ]


viewTreeNode : Model -> Zipper -> List (Html Msg)
viewTreeNode model accuZipper =
    let
        node =
            fst accuZipper

        datum =
            MultiwayTree.datum node

        currentZipper =
            model.currentZipperUndoList.present

        liHtml =
            li
                []
                [ let
                    isCurrentZipper =
                        (fst accuZipper) `hasSameDatumThan` (fst currentZipper)
                  in
                    if isCurrentZipper then
                        input
                            [ id selectedNodeId
                            , value datum.text
                            , let
                                eventOptions =
                                    { preventDefault = True, stopPropagation = False }

                                filterKey keyCode =
                                    if
                                        List.member keyCode [ enter, up, down, tab ]
                                            || (Char.fromCode keyCode == 'Z' && model.isCtrlDown)
                                            || (keyCode == backspace && String.isEmpty datum.text)
                                    then
                                        Ok keyCode
                                    else
                                        Err "Will be handled by input event"

                                decoder =
                                    Decode.customDecoder Html.Events.keyCode filterKey
                                        |> Decode.map
                                            (\keyCode ->
                                                if keyCode == backspace then
                                                    RemoveCurrentNodeFromBackspace
                                                else
                                                    NoOp
                                            )
                              in
                                onWithOptions "keydown" eventOptions decoder
                            , onInput SetText
                            , style
                                [ ( "border", "none" )
                                , ( "background-color", "lightblue" )
                                , ( "font-size", "1em" )
                                , ( "width", "100%" )
                                ]
                            ]
                            []
                    else
                        div
                            [ onClick (FocusNode accuZipper)
                            , style
                                [ -- Compensate input element border
                                  ( "padding", "1px 0" )
                                , ( "white-space", "pre-wrap" )
                                ]
                            ]
                            (let
                                -- From http://stackoverflow.com/questions/280712/javascript-unicode-regexes
                                unicodeCharacter =
                                    "[\\u00BF-\\u1FFF\\u2C00-\\uD7FF\\w]"
                             in
                                (fragments
                                    datum.text
                                    [ ( Regex.regex ("#" ++ unicodeCharacter ++ "+"), Tag )
                                    , ( Regex.regex ("@" ++ unicodeCharacter ++ "+"), Contact )
                                    , ( -- From Form.Validate
                                        Regex.regex "(https?://)?([\\da-z\\.-]+)\\.([a-z\\.]{2,6})([\\w \\.-]*)*/?"
                                            |> Regex.caseInsensitive
                                      , Url
                                      )
                                    ]
                                )
                                    |> List.map viewFragment
                            )
                ]

        nodeChildren =
            MultiwayTree.children node
    in
        if List.isEmpty nodeChildren then
            [ liHtml ]
        else
            [ liHtml, viewTreeNodeChildren model accuZipper ]


viewTreeNodeChildren : Model -> Zipper -> Html Msg
viewTreeNodeChildren model accuZipper =
    let
        children =
            MultiwayTree.children (fst accuZipper)
    in
        ul []
            (List.indexedMap
                (\index _ ->
                    let
                        accuZipper' =
                            MultiwayTreeZipper.goToChild index accuZipper
                                |> justOrCrash "viewTreeNodeChildren"
                    in
                        viewTreeNode model accuZipper'
                )
                children
                |> List.concat
            )


type Fragment
    = StringFragment String
    | MatchFragment MatchType String


type MatchType
    = Tag
    | Contact
    | Url


fragments : String -> List ( Regex, MatchType ) -> List Fragment
fragments str =
    let
        fromRegex : Regex -> MatchType -> String -> List Fragment
        fromRegex regex matchType str =
            let
                matches =
                    Regex.find Regex.All regex str

                fromMatches : String -> List Regex.Match -> Int -> List Fragment
                fromMatches accStr accMatches accIndex =
                    case accMatches of
                        [] ->
                            if String.isEmpty accStr then
                                []
                            else
                                [ StringFragment accStr ]

                        { match, index } :: remainingMatches ->
                            let
                                firstFragmentStr =
                                    String.left (index - accIndex) accStr
                            in
                                (if String.isEmpty firstFragmentStr then
                                    []
                                 else
                                    [ StringFragment firstFragmentStr ]
                                )
                                    ++ MatchFragment matchType match
                                    :: let
                                        remainingStr =
                                            String.slice
                                                ((index - accIndex) + String.length match)
                                                (String.length accStr)
                                                accStr
                                       in
                                        fromMatches remainingStr remainingMatches (index + String.length match)
            in
                if List.isEmpty matches then
                    [ StringFragment str ]
                else
                    fromMatches str matches 0

        initialFragments =
            [ StringFragment str ]
    in
        List.foldl
            (\( regex, matchType ) accFragments ->
                accFragments
                    |> List.concatMap
                        (\fragment ->
                            case fragment of
                                StringFragment str ->
                                    fromRegex regex matchType str

                                (MatchFragment _ _) as fragment ->
                                    [ fragment ]
                        )
            )
            initialFragments


viewFragment : Fragment -> Html msg
viewFragment fragment =
    case fragment of
        StringFragment str ->
            text
                (if String.isEmpty str then
                    -- Insert a space in the order to make the text node div selectable.
                    " "
                 else
                    str
                )

        MatchFragment matchType str ->
            let
                spanUnderline str =
                    span
                        [ style [ ( "text-decoration", "underline" ) ] ]
                        [ text str ]
            in
                case matchType of
                    Contact ->
                        spanUnderline str

                    Tag ->
                        spanUnderline str

                    Url ->
                        a [ href str, target "_blank", rel "external" ] [ text str ]


type alias ActionDescription =
    ( Char, String, String, Msg, Bool )


type ToolbarItem
    = Action ActionDescription
    | Separator


viewToolbar : Model -> Html Msg
viewToolbar model =
    let
        currentZipper =
            model.currentZipperUndoList.present

        actions =
            [ Action ( '↑', "Up", "Select the previous node", SelectPreviousNode, canGoToPrevious currentZipper )
            , Action ( '↓', "Down", "Select the next node", SelectNextNode, canGoToNext currentZipper )
            , Separator
            , Action ( '↥', "Alt-Up", "Move the selected node upwards", MoveCurrentNodeUp, canMoveUp currentZipper )
            , Action ( '↧', "Alt-Down", "Move the selected node downwards", MoveCurrentNodeDown, canMoveDown currentZipper )
            , Action ( '↤', "Shift-Tab", "Dedent the selected node", DedentCurrentNode, canDedent currentZipper )
            , Action ( '↦', "Tab", "Indent the selected node", IndentCurrentNode, canIndent currentZipper )
            , Separator
            , Action ( '↳', "Enter", "Insert a node below", InsertNodeBelow, True )
            , Action ( '✗', "Ctrl-Shift-K", "Remove the selected node", RemoveCurrentNode, canRemove currentZipper )
            , Separator
            , Action ( '↩', "Ctrl-Z", "Undo", Undo, UndoList.hasPast model.currentZipperUndoList )
            , Action ( '↪', "Ctrl-Shift-Z", "Redo", Redo, UndoList.hasFuture model.currentZipperUndoList )
            ]
    in
        p [] (List.map viewToolbarItem actions)


viewToolbarItem : ToolbarItem -> Html Msg
viewToolbarItem item =
    case item of
        Action ( symbol, keyName, label, msg, isEnabled ) ->
            button
                [ onClick msg
                , disabled (not (isEnabled))
                , title (keyName ++ " – " ++ label)
                , style [ ( "height", "2em" ), ( "width", "2em" ), ( "margin-right", "0.5em" ) ]
                ]
                [ text (String.fromChar symbol) ]

        Separator ->
            span [ style [ ( "margin-right", "1em" ) ] ] []



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
            getTreeRootFromZipper newModel.currentZipperUndoList.present
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
