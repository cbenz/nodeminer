module Maybe.Custom exposing (..)


justOrCrash : String -> Maybe a -> a
justOrCrash msg maybe =
    case maybe of
        Just value ->
            value

        Nothing ->
            Debug.crash (msg ++ ": a value should never have been `Nothing`.")
