module Helpers.KeyboardNavigation exposing (Direction(..), FocusResult, Key(..), navigateWrappedList, onKeyDown, setFocus)

import Dict
import Dom
import Html exposing (Attribute)
import Html.Events as E
import Json.Decode as Json
import Task


type FocusResult
    = FocusResult (Result Dom.Error ())


setFocus : Maybe String -> (FocusResult -> msg) -> Cmd msg
setFocus maybeId msg =
    case maybeId of
        Nothing ->
            Cmd.none

        Just id ->
            Dom.focus id
                |> Task.attempt FocusResult
                |> Cmd.map msg


type Key
    = RETURN
    | LEFT
    | UP
    | DOWN
    | RIGHT
    | DELETE
    | UNKNOWN


type Direction
    = Next
    | Previous


navigateWrappedList : List a -> a -> Direction -> Maybe a
navigateWrappedList xs currentX direction =
    let
        idxToX =
            xs
                |> List.indexedMap (,)
                |> Dict.fromList

        maybeCurrentIdx =
            xs
                |> List.indexedMap (,)
                |> List.foldl
                    (\( idx, x ) acc ->
                        if x == currentX then
                            Just idx
                        else
                            acc
                    )
                    Nothing

        lookup =
            \idx -> Dict.get idx idxToX

        maxIdx =
            List.length xs - 1

        goForward =
            addWrapped maxIdx 1 >> lookup

        goBack =
            addWrapped maxIdx -1 >> lookup
    in
    case ( maybeCurrentIdx, direction ) of
        ( Nothing, _ ) ->
            Nothing

        ( Just currentIdx, Next ) ->
            goForward currentIdx

        ( Just currentIdx, Previous ) ->
            goBack currentIdx


addWrapped : Int -> Int -> Int -> Int
addWrapped max a b =
    let
        sum =
            a + b
    in
    if sum > max then
        sum - max - 1
    else if sum < 0 then
        sum + max + 1
    else
        sum


getKey : Int -> Key
getKey keyCode =
    case keyCode of
        13 ->
            RETURN

        37 ->
            LEFT

        38 ->
            UP

        39 ->
            RIGHT

        40 ->
            DOWN

        46 ->
            DELETE

        _ ->
            UNKNOWN


onKeyDown : (Key -> msg) -> List Key -> Attribute msg
onKeyDown processKey keys =
    let
        options =
            { stopPropagation = True
            , preventDefault = True
            }

        decoder =
            E.keyCode
                |> Json.andThen
                    (\code ->
                        let
                            key =
                                getKey code
                        in
                        if List.member key keys then
                            Json.succeed (processKey key)
                        else
                            Json.fail "ignore"
                    )
    in
    E.onWithOptions "keydown"
        options
        decoder
