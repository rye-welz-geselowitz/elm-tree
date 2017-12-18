module Tree exposing (build, view)

import Date exposing (Date)
import Html exposing (Html, div, li, text, ul)
import Html.Attributes exposing (class)


type Tree comparable data
    = Tree comparable (Data data) (List (Tree comparable data))


type Data data
    = Data data


build : (item -> comparable) -> (item -> Maybe comparable) -> (item -> data) -> List item -> Maybe (Tree comparable data)
build id parentId data items =
    case List.partition (isRoot parentId) items of
        ( [ root ], rest ) ->
            Just (attachChildren id parentId data rest root)

        _ ->
            Nothing


attachChildren : (item -> comparable) -> (item -> Maybe comparable) -> (item -> data) -> List item -> item -> Tree comparable data
attachChildren id parentId data candidates item =
    let
        ( children, rest ) =
            List.partition (isChild id parentId item) candidates

        childTrees =
            List.map (attachChildren id parentId data rest) children
    in
    Tree (id item) (Data (data item)) childTrees


isChild : (item -> comparable) -> (item -> Maybe comparable) -> item -> item -> Bool
isChild id parentId item candidate =
    case parentId candidate of
        Just someId ->
            someId == id item

        Nothing ->
            False


isRoot : (item -> Maybe comparable) -> item -> Bool
isRoot parentId item =
    case parentId item of
        Nothing ->
            True

        _ ->
            False


myItems =
    [ { name = "A", id = 1, parentId = Nothing }
    , { name = "B", id = 2, parentId = Just 1 }
    , { name = "C", id = 3, parentId = Just 1 }
    , { name = "D", id = 4, parentId = Just 2 }
    , { name = "E", id = 5, parentId = Just 4 }
    ]


data : Tree comparable data -> data
data (Tree _ (Data data) _) =
    data


children : Tree comparable data -> List (Tree comparable data)
children (Tree _ _ children) =
    children


view : (data -> String) -> Tree comparable data -> Html msg
view display root =
    ul [] [ nodeView display root ]


nodeView : (data -> String) -> Tree comparable data -> Html msg
nodeView display tree =
    li [ class "sibling" ]
        [ div [] [ tree |> data |> display |> text ]
        , ul [ class "children" ] (children tree |> List.map (view display))
        ]



-- main : Html msg
-- main =
--     myItems
--         |> build .id .parentId .name
--         |> Maybe.map (view identity)
--         |> Maybe.withDefault (div [] [])
