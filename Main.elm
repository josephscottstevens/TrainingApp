module Main exposing (main)

import Html exposing (Html, program, div, img, text)
import Html.Attributes exposing (src, style, width)
import DragDrop


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )


type alias Model =
    { dragItem : DragDrop.DragItem
    , dragDrop : DragDrop.Model
    }


view : Model -> Html Msg
view model =
    let
        dropId =
            DragDrop.getDropId model.dragDrop
    in
        div []
            [ viewDiv 0 model.dragItem dropId
            , viewDiv 1 model.dragItem dropId
            , viewDiv 2 model.dragItem dropId
            ]


viewDiv : Int -> DragDrop.DragItem -> Maybe DragDrop.DragItem -> Html Msg
viewDiv itemId dragItem activeDragItem =
    let
        dropStyle =
            if Just dragItem == activeDragItem then
                []
            else
                DragDrop.droppable DragDropMsg

        divStyle =
            [ style
                [ ( "border", "1px solid black" )
                , ( "padding", "50px" )
                , ( "text-align", "center" )
                , if dragItem == Just activeDragItem then
                    ( "background-color", "cyan" )
                  else
                    ( "", "" )
                ]
            ]

        url =
            "https://upload.wikimedia.org/wikipedia/commons/f/f3/Elm_logo.svg"

        children =
            if dragItem == Just activeDragItem then
                [ img (src url :: width 100 :: DragDrop.draggable DragDropMsg dragItem) []
                , text (toString dragItem.dragId)
                ]
            else
                []
    in
        div (divStyle ++ dropStyle) children


type Msg
    = DragDropMsg DragDrop.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragDropMsg dragMsg ->
            let
                ( dragDrop, result ) =
                    DragDrop.update dragMsg model.dragDrop

                newModel =
                    { model
                        | dragDrop = dragDrop
                        , dragItem =
                            case result of
                                Nothing ->
                                    model.dragItem

                                Just t ->
                                    { dragId = t.dragId + 1, dropId = t.dropId }
                    }
            in
                ( newModel, Cmd.none )


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


emptyModel : Model
emptyModel =
    { dragItem = { dragId = 0, dropId = 0 }
    , dragDrop = DragDrop.init
    }
