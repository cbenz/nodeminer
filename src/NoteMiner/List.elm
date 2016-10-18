module NoteMiner.List exposing (..)

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
