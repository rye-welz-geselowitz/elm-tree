module App exposing (..)

import Html exposing (Html, div, text)
import Tree


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}
    , Cmd.none
    )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


myItems =
    [ { name = "A", id = 1, parentId = Nothing }
    , { name = "B", id = 2, parentId = Just 1 }
    , { name = "C", id = 3, parentId = Just 1 }
    , { name = "D", id = 4, parentId = Just 2 }
    , { name = "E", id = 5, parentId = Just 4 }
    , { name = "F", id = 6, parentId = Just 3 }
    , { name = "G", id = 7, parentId = Just 3 }
    ]


view : Model -> Html Msg
view model =
    let
        maybeTree =
            myItems
                |> Tree.build .id .parentId
    in
    case maybeTree of
        Nothing ->
            div [] []

        Just tree ->
            div []
                [ tree
                    |> Tree.map (\i -> { i | name = i.name ++ "2" })
                    |> Tree.update 3 (\i -> { i | name = "YO!!!!" })
                    |> Tree.view nodeView customCompare
                , div []
                    [ Tree.traverseDepthFirst customCompare tree
                        |> List.map .name
                        |> toString
                        |> text
                    ]
                , div []
                    [ Tree.traverseBreadthFirst customCompare tree
                        |> List.map .name
                        |> toString
                        |> text
                    ]
                ]


type alias Node =
    { name : String
    , id : Int
    , parentId : Maybe Int
    }


nodeView : Node -> Html Msg
nodeView node =
    div [] [ text node.name ]


customCompare d1 d2 =
    compare d2.name d1.name


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
