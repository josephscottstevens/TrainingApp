module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html5.DragDrop as DragDrop


type Position
    = Up
    | Middle
    | Down


type alias Model =
    { data : { count : Int, position : Position }
    , dragDrop : DragDrop.Model Int Position
    }


type Msg
    = DragDropMsg (DragDrop.Msg Int Position)


model : Model
model =
    { data = { count = 0, position = Up }
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

                            Just ( count, position ) ->
                                { count = count + 1, position = position }
                }
                    ! []


divStyle : Attribute msg
divStyle =
    style [ ( "border", "1px solid black" ), ( "padding", "50px" ), ( "text-align", "center" ) ]


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


viewDiv : Position -> { a | position : Position, count : Int } -> Maybe Position -> Html Msg
viewDiv position data dropId =
    let
        highlight =
            if dropId |> Maybe.map ((==) position) |> Maybe.withDefault False then
                [ style [ ( "background-color", "cyan" ) ] ]
            else
                []
    in
        div
            (divStyle
                :: highlight
                ++ (if data.position /= position then
                        DragDrop.droppable DragDropMsg position
                    else
                        []
                   )
            )
            (if data.position == position then
                [ img (src "https://upload.wikimedia.org/wikipedia/commons/f/f3/Elm_logo.svg" :: width 100 :: DragDrop.draggable DragDropMsg data.count) []
                , text (toString data.count)
                ]
             else
                []
            )


main : Program Never Model Msg
main =
    program
        { init = ( model, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
