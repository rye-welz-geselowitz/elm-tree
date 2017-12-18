module App exposing (..)

import Html exposing (Html, div)
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
    ]


view : Model -> Html Msg
view model =
    myItems
        |> Tree.build .id .parentId .name
        |> Maybe.map (Tree.view identity)
        |> Maybe.withDefault (div [] [])


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
