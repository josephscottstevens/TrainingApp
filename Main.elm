module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import DragDrop


type Position
    = Up
    | Middle
    | Down


type alias Model =
    { data : { dragId : Int, dropId : Position }
    , dragDrop : DragDrop.Model Int Position
    }


type Msg
    = DragDropMsg (DragDrop.Msg Int Position)


model : Model
model =
    { data = { dragId = 0, dropId = Up }
    , dragDrop = DragDrop.init
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragDropMsg dragMsg ->
            let
                ( dragDrop, result ) =
                    DragDrop.update dragMsg model.dragDrop
            in
                { model
                    | dragDrop = dragDrop
                    , data =
                        case result of
                            Nothing ->
                                model.data

                            Just ( dragId, dropId ) ->
                                { dragId = dragId + 1, dropId = dropId }
                }
                    ! []


view : Model -> Html Msg
view model =
    let
        dropId =
            DragDrop.getDropId model.dragDrop
    in
        div []
            [ viewDiv Up model.data dropId
            , viewDiv Middle model.data dropId
            , viewDiv Down model.data dropId
            ]


divStyle : List (Attribute msg)
divStyle =
    [ style [ ( "border", "1px solid black" ), ( "padding", "50px" ), ( "text-align", "center" ) ] ]


viewDiv : Position -> { dropId : Position, dragId : Int } -> Maybe Position -> Html Msg
viewDiv position data dropId =
    let
        highlight =
            if dropId == Just position then
                [ style [ ( "background-color", "cyan" ) ] ]
            else
                []

        droppable =
            if data.dropId /= position then
                DragDrop.droppable DragDropMsg position
            else
                []

        attributes =
            divStyle ++ highlight ++ droppable

        children =
            if data.dropId == position then
                [ img (src "https://upload.wikimedia.org/wikipedia/commons/f/f3/Elm_logo.svg" :: width 100 :: DragDrop.draggable DragDropMsg data.dragId) []
                , text (toString data.dragId)
                ]
            else
                []
    in
        div attributes children


main : Program Never Model Msg
main =
    program
        { init = ( model, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
