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
    = SelectionData (Dict Int SelectionDatum) Int

type alias SelectionDatum =
  {
    selected: Bool,
    order: Int
  }

--EXPOSED FUNCTIONS


buildOrdered : List Int -> (Int -> Bool) -> (Int -> Int) -> SelectionData
buildOrdered ids isSelectedFunc orderAddedFunc =
    build ids isSelectedFunc (Just orderAddedFunc)


buildUnordered : List Int -> (Int -> Bool) -> SelectionData
buildUnordered ids isSelectedFunc =
    build ids isSelectedFunc Nothing


empty : SelectionData
empty =
    SelectionData Dict.empty 0


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
isSelected id (SelectionData selectionStatusDict _ ) =
    let
      maybeDatum = (Dict.get id selectionStatusDict)
    in
    case maybeDatum of
      Nothing ->
        False
      Just datum ->
        datum.selected


compareOrderAdded : SelectionData -> (a -> Int) -> a -> a -> Order
compareOrderAdded (SelectionData selectionDict _) getId a b =
    let
        order item =
            let
              maybeDatum = (Dict.get (getId item) selectionDict)
            in
              case maybeDatum of
                Nothing ->
                  -1
                Just datum ->
                  datum.order
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
updateSelectionStatus id selectedStatus customAddedIndex (SelectionData selectionDict lastAddedIndex) =
    let

        incrementedIndex = lastAddedIndex + 1

        index =
          case (customAddedIndex, selectedStatus) of
            (Nothing, True) ->
              incrementedIndex
            (Just someIndex, True) ->
              someIndex
            (_, False) ->
              -1
        newDatum =
          {
            selected = selectedStatus,
            order = index
          }

        newSelectionStatusDict =
            Dict.insert id newDatum selectionDict

    in
    SelectionData newSelectionStatusDict index
