module Views.SelectableList exposing (Config, State, emptyState, view)

import Dom
import Entities.SelectionData as SelectionData exposing (SelectionData)
import Html exposing (Attribute, Html, button, div, h4, li, text, ul, input)
import Html.Attributes exposing (attribute, id, tabindex, classList, class, tabindex)
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
    = State (Maybe String) SelectionData String


emptyState : State
emptyState =
    State Nothing SelectionData.empty ""



view : Config msg item -> State -> Html msg
view config state =
    div []
        [ tagsView config state, listView config state ]


tagsView : Config msg item -> State -> Html msg
tagsView config state =
    div [class "tagsview"]
        [ ul [class "tagList"]
            (selectedItems config state
                |> List.map (tagView config state)
            )
        , input [id "searchbox", tabindex 0] []
        ]


tagView : Config msg item -> State -> item -> Html msg
tagView config state item =
    li [  class "tag" ]
        [ text (config.tagDisplayText item),
          button [onClick config state item False] [text "x"]
        ]


listView : Config msg item -> State -> Html msg
listView config state =
    div []
        [ ul []
            (filteredItems config state
                |> List.map (itemView config state)
            )
        ]


itemView : Config msg item -> State -> item -> Html msg
itemView config (State maybeFocusId selectionData searchText) item =
    let
      isItemSelected =
        SelectionData.isSelected (config.itemId item) selectionData
    in
    li [ onClick config (State maybeFocusId selectionData searchText)  item True,
          classList [("selected", isItemSelected)]
    ]
        [ text (config.listDisplayText item)
        ]

--Custom Events
onClick : Config msg item -> State -> item -> Bool -> Attribute msg
onClick config (State maybeFocusId selectionData searchText) item newSelectionStatus =
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
            Json.succeed (State maybeFocusId newSelectionData searchText)



--Helpers
filteredItems : Config msg item -> State -> List item
filteredItems config (State _ selectionData searchText) =
    config.items



selectedItems : Config msg item -> State -> List item
selectedItems config (State _ selectionData searchText) =
    List.filter
        (\item ->
            SelectionData.isSelected (config.itemId item) selectionData
        )
        config.items
