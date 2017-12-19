module SelectableTree exposing (SelectableTree)

import Tree exposing (Tree)

type alias SelectableTree comparable data =
   (Tree comparable (Selectable data))

type Selectable data =
  Selectable Bool data

selectOrDeselect : comparable -> Bool -> SelectableTree comparable data -> SelectableTree comparable data
selectOrDeselect itemId selectionStatus tree =
  Tree.recursiveSelection
      itemId
      (updateSelectable selectionStatus)
      (\t -> isSelected t == selectionStatus)
      tree


isSelected : Selectable data  -> Bool
isSelected (Selectable isSelected _) =
  isSelected

-- selected : SelectableTree comparable data  -> Bool
-- selected tree=
--   tree |> Tree.data |> isSelected

updateSelectable : Bool -> Selectable data -> Selectable data
updateSelectable selectionStatus (Selectable _ data) =
   (Selectable selectionStatus data)

-- updateSelectionStatus: Bool -> Selectable data -> Selectable data
-- updateSelectionStatus selectionStatus tree =
--   Tree.updateRoot (updateSelectable selectionStatus) tree
