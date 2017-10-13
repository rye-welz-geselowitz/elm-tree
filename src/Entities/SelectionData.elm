module Entities.SelectionData
    exposing
        ( SelectionData
        , buildOrdered
        , buildUnordered
        , compareOrderAdded
        , deselectById
        , empty
        , isSelected
        , selectById
        )

import Dict exposing (Dict)


type SelectionData
    = SelectionData (Dict Int Bool) (Dict Int Int) Int



--EXPOSED FUNCTIONS


buildOrdered : List Int -> (Int -> Bool) -> (Int -> Int) -> SelectionData
buildOrdered ids isSelectedFunc orderAddedFunc =
    build ids isSelectedFunc (Just orderAddedFunc)


buildUnordered : List Int -> (Int -> Bool) -> SelectionData
buildUnordered ids isSelectedFunc =
    build ids isSelectedFunc Nothing


empty : SelectionData
empty =
    SelectionData Dict.empty Dict.empty 0


selectById : Int -> SelectionData -> SelectionData
selectById id selectionData =
    case isSelected id selectionData of
        True ->
            selectionData

        False ->
            updateSelectionStatus id True Nothing selectionData


deselectById : Int -> SelectionData -> SelectionData
deselectById id selectionData =
    case isSelected id selectionData of
        True ->
            updateSelectionStatus id False Nothing selectionData

        False ->
            selectionData


isSelected : Int -> SelectionData -> Bool
isSelected id (SelectionData selectionStatusDict _ _) =
    Maybe.withDefault False (Dict.get id selectionStatusDict)


compareOrderAdded : SelectionData -> (a -> Int) -> a -> a -> Order
compareOrderAdded (SelectionData _ orderAddedDict _) getId a b =
    let
        order item =
            Maybe.withDefault 0 (Dict.get (getId item) orderAddedDict)
    in
    if order a > order b then
        GT
    else if order a == order b then
        EQ
    else
        LT



--HELPERS


build : List Int -> (Int -> Bool) -> Maybe (Int -> Int) -> SelectionData
build ids isSelectedFunc maybeOrderAddedFunc =
    List.foldl
        (\id selectionData ->
            let
                orderAdded =
                    case maybeOrderAddedFunc of
                        Just func ->
                            Just (func id)

                        Nothing ->
                            Just 0
            in
            updateSelectionStatus id (isSelectedFunc id) orderAdded selectionData
        )
        empty
        ids


updateSelectionStatus : Int -> Bool -> Maybe Int -> SelectionData -> SelectionData
updateSelectionStatus id selectedStatus customAddedIndex (SelectionData selectionStatusDict orderAddedDict lastAddedIndex) =
    let
        newSelectionStatusDict =
            Dict.insert id selectedStatus selectionStatusDict

        newLastAddedIndex =
            lastAddedIndex + 1

        newOrderAddedDict =
            Dict.insert
                id
                (Maybe.withDefault newLastAddedIndex customAddedIndex)
                orderAddedDict
    in
    SelectionData newSelectionStatusDict newOrderAddedDict newLastAddedIndex
