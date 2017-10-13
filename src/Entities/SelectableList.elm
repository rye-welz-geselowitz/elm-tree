module Entities.SelectableList
    exposing
        ( Config
        , State
        , emptyState
        , filteredItems
        , isItemActive
        , isItemSelected
        , onClick
        , onInput
        , onKeyDown
        , searchText
        , selectedItems
        , setFocus
        , tagElementId
        )

import Entities.SelectionData as SelectionData exposing (SelectionData)
import Helpers.KeyboardNavigation as KeyboardNavigation exposing (Direction(..), FocusResult, Key(..))
import Html exposing (Attribute, Html, button, div, h4, input, li, text, ul)
import Html.Events as E
import Json.Decode as Json


---------------EXPOSED------------------
-- CONFIG && STATE


type alias Config msg item =
    { toMsg : State -> msg
    , listDisplayText : item -> String
    , tagDisplayText : item -> String
    , itemId : item -> Int
    , items : List item
    , filterFunction : String -> item -> Bool
    }


type State
    = State (Maybe Int) (Maybe String) SelectionData String


emptyState : State
emptyState =
    State Nothing Nothing SelectionData.empty ""



--DISPLAY


filteredItems : Config msg item -> State -> List item
filteredItems config (State _ _ selectionData searchText) =
    config.items
        |> List.filter (config.filterFunction searchText)


selectedItems : Config msg item -> State -> List item
selectedItems config (State _ _ selectionData searchText) =
    let
        filterItem =
            config.itemId >> SelectionData.isSelected selectionData

        compareOrderAdded =
            SelectionData.compareOrderAdded selectionData config.itemId
    in
    config.items
        |> List.filter filterItem
        |> List.sortWith compareOrderAdded


searchText : State -> String
searchText (State _ _ _ searchText) =
    searchText


isItemSelected : Config msg item -> State -> (item -> Bool)
isItemSelected config state =
    config.itemId >> isItemIdSelected config state


isItemActive : Config msg item -> State -> item -> Bool
isItemActive config state item =
    case getMaybeActiveId state of
        Nothing ->
            False

        Just activeId ->
            config.itemId item == activeId


tagElementId : Int -> String
tagElementId idx =
    "tag-" ++ toString idx



--HTML EVENTS


onClick : Config msg item -> State -> item -> Bool -> Attribute msg
onClick config state item newSelectionStatus =
    let
        newState =
            case newSelectionStatus of
                True ->
                    selectItem config state (config.itemId item)

                False ->
                    deselectItem config (config.itemId item) state
    in
    E.on "click" <|
        Json.map config.toMsg <|
            Json.succeed newState


onInput : Config msg item -> State -> Attribute msg
onInput config state =
    let
        updateFunc str =
            let
                filteredState =
                    updateSearchText str state
            in
            state
                |> updateSearchText str
                |> setActiveItemOnFilter config
    in
    E.on "input" <|
        Json.map config.toMsg <|
            Json.map updateFunc E.targetValue


onKeyDown : Config msg item -> State -> Attribute msg
onKeyDown config state =
    KeyboardNavigation.onKeyDown (handleKeyDown config state Nothing) validKeys


setFocus : State -> (FocusResult -> msg) -> Cmd msg
setFocus state postFocus =
    KeyboardNavigation.setFocus (getMaybeFocusId state) postFocus



---------------HIDDEN------------------
--EVENT LOGIC


handleKeyDown : Config msg item -> State -> Maybe item -> Key -> msg
handleKeyDown config state maybeCurrentItem key =
    let
        newState =
            case key of
                RETURN ->
                    trySelectItem config state (getMaybeActiveId state)

                UP ->
                    navigateListItems config state Previous

                DOWN ->
                    navigateListItems config state Next

                LEFT ->
                    navigateTags config Previous state

                RIGHT ->
                    navigateTags config Next state

                DELETE ->
                    tryDeleteItem config state maybeCurrentItem

                _ ->
                    state
    in
    config.toMsg newState


validKeys : List Key
validKeys =
    [ RETURN, DOWN, UP, LEFT, RIGHT, DELETE ]



--KEYBOARD NAVIGATION


navigateTags : Config msg item -> Direction -> State -> State
navigateTags config direction state =
    navigateWrappedList
        config
        state
        (getMaybeFocusId state)
        (selectedItemIds config state)
        direction
        updateMaybeFocusId


navigateListItems : Config msg item -> State -> Direction -> State
navigateListItems config state direction =
    navigateWrappedList
        config
        state
        (getMaybeActiveId state)
        (filteredItems config state
            |> List.map config.itemId
        )
        direction
        updateMaybeActiveId


navigateWrappedList :
    Config msg item
    -> State
    -> Maybe a
    -> List a
    -> Direction
    -> (Maybe a -> State -> State)
    -> State
navigateWrappedList config state maybeCurrent list direction updateFunction =
    let
        maybeNew =
            case ( maybeCurrent, direction ) of
                ( Nothing, Previous ) ->
                    list |> List.reverse |> List.head

                ( Nothing, Next ) ->
                    List.head list

                ( Just current, direction ) ->
                    KeyboardNavigation.navigateWrappedList list current direction
    in
    state |> updateFunction maybeNew



--FILTERING


setActiveItemOnFilter : Config msg item -> State -> State
setActiveItemOnFilter config state =
    let
        filteredItemIds =
            filteredItems config state
                |> List.map config.itemId

        maybeFirstFilteredItemId =
            case filteredItems config state |> List.head of
                Nothing ->
                    Nothing

                Just item ->
                    Just (config.itemId item)
    in
    state |> updateMaybeActiveId maybeFirstFilteredItemId



--SELECTING


trySelectItem : Config msg item -> State -> Maybe Int -> State
trySelectItem config state maybeActiveId =
    case maybeActiveId of
        Just activeId ->
            selectItem config state activeId

        Nothing ->
            state


tryDeleteItem : Config msg item -> State -> Maybe item -> State
tryDeleteItem config state maybeCurrentItem =
    case maybeCurrentItem of
        Just currentItem ->
            state
                |> navigateTags config Previous
                |> deselectItem config (config.itemId currentItem)

        Nothing ->
            state
                |> navigateTags config Previous


selectItem : Config msg item -> State -> Int -> State
selectItem config state itemId =
    if isItemIdSelected config state itemId then
        state
    else
        state
            |> selectItemById config itemId
            |> updateSearchText ""
            |> updateMaybeFocusId (Just "searchbox")


deselectItem : Config msg item -> Int -> State -> State
deselectItem config itemId state =
    let
        selectionData =
            getSelectionData state
    in
    state
        |> deselectItemById config itemId
        |> updateMaybeActiveId Nothing
        |> updateSearchText ""


selectItemById : Config msg item -> Int -> State -> State
selectItemById config itemId state =
    let
        selectionData =
            getSelectionData state
    in
    state
        |> updateSelectionData (SelectionData.selectById itemId selectionData)


deselectItemById : Config msg item -> Int -> State -> State
deselectItemById config itemId state =
    let
        selectionData =
            getSelectionData state
    in
    state
        |> updateSelectionData (SelectionData.deselectById itemId selectionData)


isItemIdSelected : Config msg item -> State -> Int -> Bool
isItemIdSelected config state itemId =
    let
        selectionData =
            getSelectionData state
    in
    SelectionData.isSelected selectionData itemId


selectedItemIds : Config msg item -> State -> List String
selectedItemIds config state =
    selectedItems config state
        |> List.indexedMap (\idx item -> tagElementId idx)
        |> (\l -> l ++ [ "searchbox" ])



--GET/ SET


updateSearchText : String -> State -> State
updateSearchText searchText (State maybeActiveId maybeFocusId selectionData _) =
    State maybeActiveId maybeFocusId selectionData searchText


updateSelectionData : SelectionData -> State -> State
updateSelectionData selectionData (State maybeActiveId maybeFocusId _ searchText) =
    State maybeActiveId maybeFocusId selectionData searchText


updateMaybeFocusId : Maybe String -> State -> State
updateMaybeFocusId maybeFocusId (State maybeActiveId _ selectionData searchText) =
    State maybeActiveId maybeFocusId selectionData searchText


updateMaybeActiveId : Maybe Int -> State -> State
updateMaybeActiveId maybeActiveId (State _ maybeFocusId selectionData searchText) =
    State maybeActiveId maybeFocusId selectionData searchText


getSelectionData : State -> SelectionData
getSelectionData (State _ _ selectionData _) =
    selectionData


getMaybeActiveId : State -> Maybe Int
getMaybeActiveId (State maybeActiveId _ _ _) =
    maybeActiveId


getMaybeFocusId : State -> Maybe String
getMaybeFocusId (State _ maybeFocusId _ _) =
    maybeFocusId
