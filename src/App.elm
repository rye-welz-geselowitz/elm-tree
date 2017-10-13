module App exposing (..)

import Html exposing (Html, div, img, text)
import Views.SelectableList as SelectableList
import Helpers.KeyboardNavigation exposing (FocusResult)

type alias Model =
    { selectableListState : SelectableList.State
    }


type alias User =
    { id : Int
    , name : String
    }


init : String -> ( Model, Cmd Msg )
init path =
    ( { selectableListState = SelectableList.emptyState
      }
    , Cmd.none
    )


type Msg
    = SetSelectableListState SelectableList.State
      | PostFocus FocusResult


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSelectableListState newState ->
            ( { model | selectableListState = newState }
            , SelectableList.setFocus newState PostFocus
            )
        PostFocus res ->
          (model, Cmd.none)


view : Model -> Html Msg
view model =
    div []
        [ SelectableList.view listConfig model.selectableListState ]


listConfig : SelectableList.Config Msg User
listConfig =
    { toMsg = SetSelectableListState
    , itemId = \item -> item.id
    , items = itemList
    , tagDisplayText = \item -> item.name
    , listDisplayText = \item -> item.name
    , filterFunction = filterFunction
    }

filterFunction : String -> User -> Bool
filterFunction searchText item =
  String.contains (cleanString searchText) (cleanString item.name)

cleanString  str =
  str |> String.trim |> String.toLower

itemList =
    [ { id = 0, name = "Elana" }
    , { id = 1, name = "Ellie" }
    , { id = 2, name = "Bellie" }
    , { id = 3, name = "Gesellie" }
    , { id = 4, name = "Wits" }
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
