module Main exposing (main)

import Html exposing (Html, program, div, img, text)
import Html.Attributes exposing (src, style, width)
import DragDrop


type alias Model =
    { data : { dragId : Int, dropId : Int }
    , dragDrop : DragDrop.Model Int Int
    }


type Msg
    = DragDropMsg (DragDrop.Msg Int Int)


model : Model
model =
    { data = { dragId = 0, dropId = 0 }
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
            [ viewDiv 0 model.data dropId
            , viewDiv 1 model.data dropId
            , viewDiv 2 model.data dropId
            ]


viewDiv : Int -> { dropId : Int, dragId : Int } -> Maybe Int -> Html Msg
viewDiv id data maybeDropId =
    let
        dropStyle =
            if data.dropId == id then
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
            if data.dropId == id then
                [ img (src "https://upload.wikimedia.org/wikipedia/commons/f/f3/Elm_logo.svg" :: width 100 :: DragDrop.draggable DragDropMsg data.dragId) []
                , text (toString data.dragId)
                ]
            else
                []
    in
        div (divStyle ++ dropStyle) children


main : Program Never Model Msg
main =
    program
        { init = ( model, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
