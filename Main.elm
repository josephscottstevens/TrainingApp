module Main exposing (main)

import Html exposing (Html, program, div, img, text)
import Html.Attributes exposing (src, style, width)
import DragDrop


type alias DragItem =
    { dragId : Int
    , dropId : Int
    }


type alias Model =
    { dragItem : DragItem
    , dragDrop : DragDrop.Model Int Int
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


viewDiv : Int -> DragItem -> Maybe Int -> Html Msg
viewDiv id dragItem maybeDropId =
    let
        dropStyle =
            if dragItem.dropId == id then
                []
            else
                DragDrop.droppable DragDropMsg id

        divStyle =
            [ style
                [ ( "border", "1px solid black" )
                , ( "padding", "50px" )
                , ( "text-align", "center" )
                , if maybeDropId == Just id then
                    ( "background-color", "cyan" )
                  else
                    ( "", "" )
                ]
            ]

        children =
            if dragItem.dropId == id then
                [ img (src "https://upload.wikimedia.org/wikipedia/commons/f/f3/Elm_logo.svg" :: width 100 :: DragDrop.draggable DragDropMsg dragItem.dragId) []
                , text (toString dragItem.dragId)
                ]
            else
                []
    in
        div (divStyle ++ dropStyle) children


init : ( Model, Cmd Msg )
init =
    { dragItem = { dragId = 0, dropId = 0 }
    , dragDrop = DragDrop.init
    }
        ! []


type Msg
    = DragDropMsg (DragDrop.Msg Int Int)


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
                    , dragItem =
                        case result of
                            Nothing ->
                                model.dragItem

                            Just ( dragId, dropId ) ->
                                { dragId = dragId + 1, dropId = dropId }
                }
                    ! []


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
