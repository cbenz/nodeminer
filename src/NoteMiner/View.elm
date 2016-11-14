module NoteMiner.View exposing (..)

import Char
import String
import Json.Decode as Decode
import Regex exposing (Regex)
import Html exposing (..)
import Html.Attributes exposing (disabled, href, id, placeholder, rel, style, target, title, type', value)
import Html.Events exposing (on, onClick, onInput, onWithOptions)
import MultiwayTree
import MultiwayTreeZipper
import UndoList
import NoteMiner.Constants exposing (selectedNodeIdHtmlAttribute)
import NoteMiner.Keyboard exposing (..)
import NoteMiner.Maybe exposing (justOrCrash)
import NoteMiner.Tree exposing (..)
import NoteMiner.Types exposing (Model, Msg(..), getSelectedNodeZipper)


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "NoteMiner" ]
        , p [] (viewToolbarItems model ++ [ separatorSpan, viewSearchInput ])
        , div
            [ style [ ( "font-family", "sans-serif" ), ( "font-size", "1em" ) ]
            ]
            [ if String.isEmpty model.searchText then
                viewTree model
              else
                viewSearchResults model
            ]
        , hr [] []
        , button [ onClick ResetToSampleTree ] [ text "Reset to sample tree" ]
        , p []
            [ "The tree is stored and synchronized in localStorage\n            . "
                ++ "Look at the JavaScript console for error messages."
                |> text
            ]
        ]


viewTree : Model -> Html Msg
viewTree model =
    div
        []
        [ viewTreeNodeChildren model (initialZipper model.treeUndoList.present) ]


viewTreeNode : Model -> Zipper -> List (Html Msg)
viewTreeNode model accuZipper =
    let
        node =
            fst accuZipper

        datum =
            MultiwayTree.datum node

        liHtml =
            li
                []
                [ if datum.id == model.selectedNodeId then
                    viewSelectedTreeNode model datum
                  else
                    viewUnselectedTreeNode datum
                ]

        nodeChildren =
            MultiwayTree.children node
    in
        if List.isEmpty nodeChildren then
            [ liHtml ]
        else
            [ liHtml, viewTreeNodeChildren model accuZipper ]


viewSelectedTreeNode : Model -> Datum -> Html Msg
viewSelectedTreeNode model datum =
    input
        [ id selectedNodeIdHtmlAttribute
        , value datum.text
        , let
            eventOptions =
                { preventDefault = True, stopPropagation = False }

            filterKey keyCode =
                if
                    List.member keyCode [ enter, up, down, tab, ctrl, alt, shift ]
                        || (model.isCtrlDown && Char.fromCode keyCode == 'Z')
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
                            else if keyCode == up then
                                if model.isAltDown then
                                    MoveCurrentNodeUp
                                else
                                    SelectPreviousNode
                            else if keyCode == down then
                                if model.isAltDown then
                                    MoveCurrentNodeDown
                                else
                                    SelectNextNode
                            else if keyCode == enter then
                                InsertNodeBelow
                            else if keyCode == tab then
                                if model.isShiftDown then
                                    DedentCurrentNode
                                else
                                    IndentCurrentNode
                            else if Char.fromCode keyCode == 'K' && model.isCtrlDown && model.isShiftDown then
                                RemoveCurrentNode
                            else if Char.fromCode keyCode == 'Z' && model.isCtrlDown then
                                if model.isShiftDown then
                                    Redo
                                else
                                    Undo
                            else if keyCode == alt then
                                ChangeModifierKey Alt True
                            else if keyCode == ctrl then
                                ChangeModifierKey Ctrl True
                            else if keyCode == shift then
                                ChangeModifierKey Shift True
                            else
                                NoOp
                        )
          in
            onWithOptions "keydown" eventOptions decoder
        , let
            decoder =
                Decode.map
                    (\keyCode ->
                        if keyCode == alt then
                            ChangeModifierKey Alt False
                        else if keyCode == ctrl then
                            ChangeModifierKey Ctrl False
                        else if keyCode == shift then
                            ChangeModifierKey Shift False
                        else
                            NoOp
                    )
                    Html.Events.keyCode
          in
            on "keyup" decoder
        , onInput SetText
        , style
            [ ( "border", "none" )
            , ( "background-color", "lightblue" )
            , ( "font-size", "1em" )
            , ( "width", "100%" )
            ]
        ]
        []


viewUnselectedTreeNode : Datum -> Html Msg
viewUnselectedTreeNode datum =
    div
        [ onClick (SelectNode datum.id)
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


viewSearchResults : Model -> Html Msg
viewSearchResults model =
    let
        datums =
            MultiwayTree.foldl
                (\datum accuList ->
                    if
                        Regex.contains
                            (Regex.regex model.searchText |> Regex.caseInsensitive)
                            datum.text
                    then
                        accuList ++ [ datum ]
                    else
                        accuList
                )
                []
                model.treeUndoList.present
    in
        ul
            []
            (List.map
                (\datum ->
                    li
                        []
                        [ if datum.id == model.selectedNodeId then
                            viewSelectedTreeNode model datum
                          else
                            viewUnselectedTreeNode datum
                        ]
                )
                datums
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


viewToolbarItems : Model -> List (Html Msg)
viewToolbarItems model =
    let
        selectedNodeZipper =
            getSelectedNodeZipper model

        actions =
            [ Action ( '↑', "Up", "Select the previous node", SelectPreviousNode, canGoToPrevious selectedNodeZipper )
            , Action ( '↓', "Down", "Select the next node", SelectNextNode, canGoToNext selectedNodeZipper )
            , Separator
            , Action ( '↥', "Alt-Up", "Move the selected node upwards", MoveCurrentNodeUp, canMoveUp selectedNodeZipper )
            , Action ( '↧', "Alt-Down", "Move the selected node downwards", MoveCurrentNodeDown, canMoveDown selectedNodeZipper )
            , Action ( '↤', "Shift-Tab", "Dedent the selected node", DedentCurrentNode, canDedent selectedNodeZipper )
            , Action ( '↦', "Tab", "Indent the selected node", IndentCurrentNode, canIndent selectedNodeZipper )
            , Separator
            , Action ( '↳', "Enter", "Insert a node below", InsertNodeBelow, True )
            , Action ( '✗', "Ctrl-Shift-K", "Remove the selected node", RemoveCurrentNode, canRemove selectedNodeZipper )
            , Separator
            , Action ( '↩', "Ctrl-Z", "Undo", Undo, UndoList.hasPast model.treeUndoList )
            , Action ( '↪', "Ctrl-Shift-Z", "Redo", Redo, UndoList.hasFuture model.treeUndoList )
            ]
    in
        List.map (viewToolbarItem model.searchText) actions


viewToolbarItem : String -> ToolbarItem -> Html Msg
viewToolbarItem searchText item =
    case item of
        Action ( symbol, keyName, label, msg, isEnabled ) ->
            button
                [ onClick msg
                , disabled (not isEnabled)
                , title (keyName ++ " – " ++ label)
                , style [ ( "height", "2em" ), ( "width", "2em" ), ( "margin-right", "0.5em" ) ]
                ]
                [ text (String.fromChar symbol) ]

        Separator ->
            separatorSpan


separatorSpan : Html a
separatorSpan =
    span [ style [ ( "margin-right", "1em" ) ] ] []


viewSearchInput : Html Msg
viewSearchInput =
    input
        [ onInput SetSearchText
        , placeholder "Search"
        , type' "search"
        ]
        []
