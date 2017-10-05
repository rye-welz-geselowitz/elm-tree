module App exposing (..)

import Components.Pattern1 as Pattern1
import Html exposing (Html, div, img, text)


type alias Model =
    {}


init : String -> ( Model, Cmd Msg )
init path =
    ( {}, Cmd.none )


type Msg
    = ExecuteCmd (Cmd Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExecuteCmd cmd ->
            let
                d =
                    Debug.log "hi" "hullo"
            in
            ( model, cmd )


view : Model -> Html Msg
view model =
    div []
        [ Pattern1.view { executeCmd = ExecuteCmd }
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
