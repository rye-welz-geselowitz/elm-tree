module Views.SelectableList exposing (Config, State, emptyState, setFocus, view)

import Entities.SelectionData as SelectionData exposing (SelectionData)
import Helpers.KeyboardNavigation as KeyboardNavigation exposing (Direction(..), FocusResult, Key(..))
import Html exposing (Attribute, Html, button, div, h4, input, li, text, ul)
import Html.Attributes exposing (attribute, class, classList, id, tabindex, placeholder, value)
import Html.Events as E
import Json.Decode as Json


type alias Config msg item =
    { toMsg : State -> msg
    , listDisplayText : item -> String
    , tagDisplayText : item -> String
    , itemId : item -> Int
    , items : List item
    , filterFunction : String -> item -> Bool
    }


setFocus : State -> (FocusResult -> msg) -> Cmd msg
setFocus state postFocus =
    KeyboardNavigation.setFocus (getMaybeFocusId state) postFocus


type State
    = State (Maybe Int) (Maybe String) SelectionData String


emptyState : State
emptyState =
    State Nothing Nothing SelectionData.empty ""


view : Config msg item -> State -> Html msg
view config state =
    div [class "selectablelist"]
        [ tagsView config state,
          listView config state ]


tagsView : Config msg item -> State -> Html msg
tagsView config state =
    div [ class "tagsview" ]
        [ ul [ class "tagList" ]
            (selectedItems config state
                |> List.indexedMap (\idx item -> tagView config state idx item)
            )
        , input
            [ id "searchbox"
            , placeholder "Search..."
            , tabindex 0
            , value (getSearchText state)
            , onInput config state
            , KeyboardNavigation.onKeyDown (handleKeyDown config state Nothing) validKeys
            ]
            []
        ]


validKeys : List Key
validKeys =
    [ RETURN, DOWN, UP, LEFT, RIGHT, DELETE ]



tagView : Config msg item -> State -> Int -> item -> Html msg
tagView config state idx item =
    li
      [ class "tag"
        , id (tagElementId idx)
        , tabindex -1
        , KeyboardNavigation.onKeyDown (handleKeyDown config state (Just item)) validKeys
        ]
        [ text (config.tagDisplayText item)
        , button [ onClick config state item False ] [ text "x" ]
        ]


listView : Config msg item -> State -> Html msg
listView config state =
    div []
        [ ul [class "itemslist"]
            (filteredItems config state
                |> List.map (itemView config state)
            )
        ]


itemView : Config msg item -> State -> item -> Html msg
itemView config state item =
    let
        isItemSelected =
            SelectionData.isSelected (config.itemId item) (getSelectionData state)

        isItemActive =
            case getMaybeActiveId state of
                Nothing ->
                    False

                Just activeId ->
                    config.itemId item == activeId
    in
    li
        [ onClick config state item True
        , classList [ ( "selected", isItemSelected ), ( "active", isItemActive ), ("item", True) ]
        ]
        [ text (config.listDisplayText item)
        ]



--Custom Events


onClick : Config msg item -> State -> item -> Bool -> Attribute msg
onClick config state item newSelectionStatus =
    let
        itemId =
            config.itemId item

        newState =
            case newSelectionStatus of
                True ->
                    selectItem config state itemId

                False ->
                    deselectItem config state itemId
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

            filteredItemIds =
              filteredItems config filteredState
              |> List.map config.itemId

            currentMaybeActiveId =
              getMaybeActiveId state

            maybeFirstFilteredItemId =
              case filteredItems config filteredState |> List.head of
                Nothing -> Nothing
                Just item ->
                  Just (config.itemId item)

            maybeActiveItemId =
              case (currentMaybeActiveId, maybeFirstFilteredItemId) of
                (Nothing, maybeFirstFilteredItemId) ->
                  maybeFirstFilteredItemId
                (Just itemId, maybeFirstFilteredItemId) ->
                  if List.member itemId filteredItemIds then
                    Just itemId
                  else
                    maybeFirstFilteredItemId



          in
            filteredState
              |> updateMaybeActiveId maybeActiveItemId

    in
    E.on "input" <|
        Json.map config.toMsg <|
            Json.map updateFunc E.targetValue


handleKeyDown : Config msg item -> State -> Maybe item -> Key -> msg
handleKeyDown config state maybeCurrentItem key =
    let
        maybeActiveId =
            getMaybeActiveId state

        newState =
            case key of
                RETURN ->
                    case maybeActiveId of
                        Just activeId ->
                            selectItem config state activeId

                        Nothing ->
                            state

                UP ->
                    navigateListItems config state Previous

                DOWN ->
                    navigateListItems config state Next

                LEFT ->
                    navigateTags config state Previous

                RIGHT ->
                    navigateTags config state Next

                DELETE ->
                    let
                        newSelection =
                            case maybeCurrentItem of
                                Just currentItem ->
                                    deselectItem config state (config.itemId currentItem)

                                Nothing ->
                                    state
                    in
                    navigateTags config state Previous
                        |> (\state ->
                                case maybeCurrentItem of
                                    Just currentItem ->
                                        deselectItem config state (config.itemId currentItem)

                                    Nothing ->
                                        state
                           )

                _ ->
                    state
    in
    config.toMsg newState


navigateTags : Config msg item -> State -> Direction -> State
navigateTags config state direction =
    let
        maybeCurrentFocusId =
            getMaybeFocusId state

        selectedIds =
            selectedItems config state
                |> List.indexedMap (\idx item -> tagElementId idx)
                |> (\l -> l ++ [ "searchbox" ])

        newMaybeFocusId =
            case ( maybeCurrentFocusId, direction ) of
                ( Nothing, Previous ) ->
                    selectedIds |> List.reverse |> List.head

                ( Nothing, Next ) ->
                    List.head selectedIds

                ( Just currentId, direction ) ->
                    KeyboardNavigation.navigateWrappedList selectedIds currentId direction
    in
    state
        |> updateMaybeFocusId newMaybeFocusId


navigateListItems : Config msg item -> State -> Direction -> State
navigateListItems config state direction =
    let
        maybeActiveId =
            getMaybeActiveId state

        filteredIds =
            filteredItems config state
                |> List.map (\item -> config.itemId item)

        newMaybeActiveId =
            case ( maybeActiveId, direction ) of
                ( Nothing, Previous ) ->
                    filteredIds |> List.reverse |> List.head

                ( Nothing, Next ) ->
                    List.head filteredIds

                ( Just currentId, direction ) ->
                    KeyboardNavigation.navigateWrappedList filteredIds currentId direction
    in
    state
        |> updateMaybeActiveId newMaybeActiveId



--update


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

getSearchText : State -> String
getSearchText (State _ _ _ searchText) =
    searchText


--Helpers


selectItem : Config msg item -> State -> Int -> State
selectItem config state itemId =
    let
        selectionData =
            getSelectionData state
    in
    case SelectionData.isSelected itemId selectionData of
      True ->
        state
      False ->
        state
            |> updateSelectionData (SelectionData.selectById itemId selectionData)
            |> updateSearchText ""
            |> updateMaybeFocusId (Just "searchbox")


deselectItem : Config msg item -> State -> Int -> State
deselectItem config state itemId =
    let
        selectionData =
            getSelectionData state
    in
    state
        |> updateSelectionData (SelectionData.deselectById itemId selectionData)
        |> updateMaybeActiveId Nothing
        |> updateSearchText ""


filteredItems : Config msg item -> State -> List item
filteredItems config (State _ _ selectionData searchText) =
    config.items
        |> List.filter (config.filterFunction searchText)


tagElementId : Int -> String
tagElementId idx =
    "tag-" ++ toString idx


selectedItems : Config msg item -> State -> List item
selectedItems config (State _ _ selectionData searchText) =
    config.items
        |> List.filter
            (\item ->
                SelectionData.isSelected (config.itemId item) selectionData
            )
        |> List.sortWith (SelectionData.compareOrderAdded selectionData config.itemId)
