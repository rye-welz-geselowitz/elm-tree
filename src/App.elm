module App exposing (..)

import Html exposing (Html, div, text, input)
import Html.Attributes exposing (type_, checked)
import Tree exposing (Tree)
import Html.Events exposing (onClick)
import SelectableTree exposing (SelectableTree)

type alias Model =
    {
    maybeTree: Maybe (Tree Int Node)
  }


init : ( Model, Cmd Msg )
init =
    ( {maybeTree  = myItems |> Tree.build .id .parentId}
    , Cmd.none
    )


type Msg
    = ToggleSelect Node


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleSelect node ->
            let
              selectStatus = not node.selected

              select = Tree.recursiveSelection node.id
                (\n -> {n | selected = selectStatus})
                (\n -> n.selected == selectStatus)

              tree = model.maybeTree
                      |> Maybe.map select
            in
            ( { model | maybeTree = tree}, Cmd.none )


myItems =
    [ Node "A" 1 Nothing False,
      Node "B" 2 (Just 1) False,
      Node "C" 3 (Just 1) False,
      Node "D" 4 (Just 2) False,
      Node "E" 5 (Just 4) False,
      Node "F" 6 (Just 3) False,
      Node "G" 7 (Just 3) False
    ]


view : Model -> Html Msg
view model =
    case model.maybeTree of
        Nothing ->
            div [] []

        Just tree ->
            div []
                [ tree
                    |> Tree.view nodeView customCompare
                , div []
                    [ Tree.flattenDepthFirst customCompare tree
                        |> List.map .name
                        |> toString
                        |> text
                    ]
                , div []
                    [ Tree.flattenBreadthFirst customCompare tree
                        |> List.map .name
                        |> toString
                        |> text
                    ]
                ]


type alias Node =
    { name : String
    , id : Int
    , parentId : Maybe Int
    , selected : Bool
    }


nodeView : Node -> Html Msg
nodeView node =
    div [] [
    input [type_ "checkbox",
          checked node.selected,
          onClick (ToggleSelect node)

    ] [],
    text node.name]


customCompare d1 d2 =
    compare d2.name d1.name


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
