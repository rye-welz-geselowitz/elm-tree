module Views.SelectableList exposing (view)

import Entities.SelectableList
    exposing
        ( Config
        , State
        , filteredItems
        , isItemActive
        , isItemSelected
        , onClick
        , onInput
        , onKeyDown
        , searchText
        , selectedItems
        , tagElementId
        )
import Html exposing (Attribute, Html, button, div, h4, input, li, text, ul)
import Html.Attributes exposing (attribute, class, classList, id, placeholder, tabindex, value)


view : Config msg item -> State -> Html msg
view config state =
    div [ class "selectablelist" ]
        [ tagsView config state
        , listView config state
        ]


tagsView : Config msg item -> State -> Html msg
tagsView config state =
    div [ class "tagsview" ]
        [ ul [ class "tagList" ]
            (selectedItems config state
                |> List.indexedMap
                    (\idx item ->
                        tagView config state idx item
                    )
            )
        , input
            [ id "searchbox"
            , placeholder "Search..."
            , tabindex 0
            , value (searchText state)
            , onInput config state
            , onKeyDown config state
            ]
            []
        ]


tagView : Config msg item -> State -> Int -> item -> Html msg
tagView config state idx item =
    li
        [ class "tag"
        , id (tagElementId idx)
        , tabindex -1
        , onKeyDown config state
        ]
        [ text (config.tagDisplayText item)
        , button [ onClick config state item False ] [ text "x" ]
        ]


listView : Config msg item -> State -> Html msg
listView config state =
    div []
        [ ul [ class "itemslist" ]
            (filteredItems config state
                |> List.map (itemView config state)
            )
        ]


itemView : Config msg item -> State -> item -> Html msg
itemView config state item =
    li
        [ onClick config state item True
        , classList
            [ ( "selected", isItemSelected config state item )
            , ( "active", isItemActive config state item )
            , ( "item", True )
            ]
        ]
        [ text (config.listDisplayText item)
        ]
