port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, form, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput, onSubmit)



-- MAIN


main : Program (Maybe Model) Model Msg
main =
    Browser.element
        { init = init
        , update = updateWithStorage
        , view = view
        , subscriptions = \_ -> Sub.none
        }


-- PORTS


port setStorage : Model -> Cmd msg

updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ setStorage newModel, cmds ]
        )



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


init : (Maybe Model) -> (Model, Cmd Msg)
init localData =
    case localData of
        Just data ->
            (data, Cmd.none)
        Nothing ->
            ({ nextId = 0
            , input = ""
            , list = []
            }, Cmd.none)



-- UPDATE


type Msg
    = Create
    | Delete Int
    | Input String
    | NoOp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Create ->
            ({ model | list =
                model.list
                    ++ [ { id = model.nextId, text = model.input } ]
                , input = ""
                , nextId = model.nextId + 1
            }, Cmd.none)

        Delete toDelete ->
            ({ model
                | list = List.filter (\item -> item.id /= toDelete) model.list
            }, Cmd.none)

        Input text ->
            ({ model | input = text }, Cmd.none)

        NoOp ->
            (model, Cmd.none)


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
