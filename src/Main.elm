module Main exposing (..)

import Browser
import Html exposing (Html, button, div, form, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput, onSubmit)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias ToDo =
    { id : Int
    , text : String
    }


type alias Model =
    { nextId : Int
    , input : String
    , list : List ToDo
    }


init : Model
init =
    { nextId = 0
    , input = ""
    , list = []
    }



-- UPDATE


type Msg
    = Create
    | Delete Int
    | Input String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Create ->
            { model | list =
                model.list
                    ++ [ { id = model.nextId, text = model.input } ]
                , input = ""
                , nextId = model.nextId + 1
            }

        Delete toDelete ->
            { model
                | list = List.filter (\item -> item.id /= toDelete) model.list
            }

        Input text ->
            { model | input = text }



-- VIEW


view : Model -> Html Msg
view model =
    div [] (List.map toDoItem model.list ++ [ addForm model.input ])


toDoItem : ToDo -> Html Msg
toDoItem data =
    div []
        [ div [] [ text data.text ]
        , button [ onClick (Delete data.id) ] [ text "Delete" ]
        ]


addForm : String -> Html Msg
addForm currentValue =
    form [ onSubmit Create ]
        [ input
            [ placeholder "New item"
            , onInput Input
            , value currentValue
            ]
            []
        , button [] [ text "Add" ]
        ]
