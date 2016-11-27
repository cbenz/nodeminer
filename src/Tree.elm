module Tree exposing (..)

import List.Custom exposing (insertAtIndex, removeAtIndex)
import Maybe.Custom exposing (justOrCrash)
import MultiwayTree
import MultiwayTreeZipper
import String


type alias NodeId =
    Int


type alias Datum =
    { text : String
    , id : NodeId
    }


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


nextNode : Tree -> Tree
nextNode tree =
    node "" (nextId tree) []


hasSameDatumThan : MultiwayTree.Tree a -> MultiwayTree.Tree a -> Bool
hasSameDatumThan a b =
    MultiwayTree.datum a == MultiwayTree.datum b


getText : Zipper -> String
getText =
    Tuple.first >> MultiwayTree.datum >> .text


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
                |> Maybe.andThen (MultiwayTreeZipper.goToChild 0)
                |> justOrCrash "isFirstVisibleNode"
    in
        zipper == firstVisibleNode


isFirstLevelNode : MultiwayTreeZipper.Zipper a -> Bool
isFirstLevelNode zipper =
    Maybe.map2 (\parent root -> MultiwayTreeZipper.datum parent == MultiwayTreeZipper.datum root)
        (MultiwayTreeZipper.goUp zipper)
        (MultiwayTreeZipper.goToRoot zipper)
        |> justOrCrash "isFirstLevelNode"


hasChildren : MultiwayTreeZipper.Zipper a -> Bool
hasChildren zipper =
    zipper |> Tuple.first |> MultiwayTree.children |> not << List.isEmpty


getTreeRootFromZipper : MultiwayTreeZipper.Zipper a -> MultiwayTree.Tree a
getTreeRootFromZipper zipper =
    MultiwayTreeZipper.goToRoot zipper
        |> justOrCrash "getTreeRootFromZipper"
        |> Tuple.first


getSiblings : MultiwayTreeZipper.Zipper a -> MultiwayTree.Forest a
getSiblings zipper =
    let
        parent =
            MultiwayTreeZipper.goUp zipper
                |> justOrCrash "getSiblings"
    in
        MultiwayTree.children (Tuple.first parent)


findIndexInForest : MultiwayTreeZipper.Zipper a -> MultiwayTree.Forest a -> Int
findIndexInForest zipper forest =
    let
        node =
            Tuple.first zipper
    in
        forest
            |> List.indexedMap
                (\index sibling ->
                    if hasSameDatumThan sibling node then
                        Just index
                    else
                        Nothing
                )
            |> List.filterMap identity
            |> List.head
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
            MultiwayTree.children (Tuple.first zipper)

        children_ =
            insertAtIndex newTree index children
    in
        MultiwayTreeZipper.updateChildren children_ zipper


{-| Inserts a `Tree` as the sibling of `index` of the `Tree` of the current focus. Does not move the focus.
-}
insertSiblingAtIndex : MultiwayTree.Tree a -> Int -> MultiwayTreeZipper.Zipper a -> Maybe (MultiwayTreeZipper.Zipper a)
insertSiblingAtIndex newTree index zipper =
    MultiwayTreeZipper.goUp zipper
        |> Maybe.andThen (insertChildAtIndex newTree index)
        |> Maybe.andThen
            -- Restore focus moved by `goUp` to its original position.
            (MultiwayTreeZipper.goToChild (index - 1))


{-| Removes the `Tree` at the current focus. Moves the focus to the parent.
-}
removeCurrentAndGoUp : MultiwayTreeZipper.Zipper a -> Maybe (MultiwayTreeZipper.Zipper a)
removeCurrentAndGoUp zipper =
    let
        index =
            findIndexInSiblings zipper

        siblings =
            getSiblings zipper

        children_ =
            removeAtIndex index siblings
    in
        MultiwayTreeZipper.goUp zipper
            |> Maybe.andThen (MultiwayTreeZipper.updateChildren children_)


goToNextSibling : MultiwayTreeZipper.Zipper a -> Maybe (MultiwayTreeZipper.Zipper a)
goToNextSibling zipper =
    let
        index =
            findIndexInSiblings zipper
    in
        MultiwayTreeZipper.goUp zipper
            |> Maybe.andThen (MultiwayTreeZipper.goToChild (index + 1))
