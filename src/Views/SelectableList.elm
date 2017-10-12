module Views.SelectableList exposing (Config, State, emptyState, view)

import Dom
import Entities.SelectionData as SelectionData exposing (SelectionData)
import Html exposing (Attribute, Html, button, div, h4, li, text, ul)
import Html.Attributes exposing (attribute, id, tabindex)
import Html.Events as E
import Json.Decode as Json
import Task


type alias Config msg item =
    { toMsg : State -> msg
    , listDisplayText : item -> String
    , tagDisplayText : item -> String
    , itemId : item -> Int
    , items : List item
    }


type State
    = State (Maybe String) SelectionData


emptyState : State
emptyState =
    State Nothing SelectionData.empty


onClick : Config msg item -> State -> item -> Bool -> Attribute msg
onClick config (State maybeFocusId selectionData) item newSelectionStatus =
    let
        itemId =
            config.itemId item

        newSelectionData =
            case newSelectionStatus of
                True ->
                    SelectionData.selectById itemId selectionData

                False ->
                    SelectionData.deselectById itemId selectionData
    in
    E.on "click" <|
        Json.map config.toMsg <|
            Json.succeed (State maybeFocusId newSelectionData)


view : Config msg item -> State -> Html msg
view config state =
    div []
        [ tagsView config state, listView config state ]


tagsView : Config msg item -> State -> Html msg
tagsView config state =
    div []
        [ h4 [] [ text "Selected" ]
        , ul []
            (selectedItems config state
                |> List.map (tagView config state)
            )
        ]


tagView : Config msg item -> State -> item -> Html msg
tagView config state item =
    li [ onClick config state item False ]
        [ text (config.tagDisplayText item)
        ]


listView : Config msg item -> State -> Html msg
listView config state =
    div []
        [ h4 [] [ text "All" ]
        , ul []
            (filteredItems config state
                |> List.map (itemView config state)
            )
        ]


itemView : Config msg item -> State -> item -> Html msg
itemView config state item =
    li [ onClick config state item True ]
        [ text (config.listDisplayText item)
        ]


filteredItems : Config msg item -> State -> List item
filteredItems config (State _ selectionData) =
    config.items


selectedItems : Config msg item -> State -> List item
selectedItems config (State _ selectionData) =
    List.filter
        (\item ->
            SelectionData.isSelected (config.itemId item) selectionData
        )
        config.items
