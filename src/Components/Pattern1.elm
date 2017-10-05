module Components.Pattern1 exposing (..)

import Dom
import Html exposing (Attribute, Html, button, div, img, text)
import Html.Attributes exposing (attribute, id, tabindex)
import Html.Events as E
import Json.Decode as Json
import Task


type alias Config msg =
    { executeCmd : Cmd msg -> msg
    }


onClick : Config msg -> Attribute msg
onClick config =
    let
        setFocus =
            Dom.focus
                "focusDiv"
                |> Task.attempt
                    (\result ->
                        case result of
                            Err (Dom.NotFound id) ->
                                config.executeCmd Cmd.none

                            Ok () ->
                                config.executeCmd Cmd.none
                    )
    in
    E.on "click"
        (Json.succeed
            (config.executeCmd
                setFocus
            )
        )


view : Config msg -> Html msg
view config =
    div []
        [ button [ onClick config ] [ text "Focus" ]
        , div
            [ id "focusDiv"
            , attribute "role" "text"
            , tabindex 0
            ]
            [ text "Hello!" ]
        ]
