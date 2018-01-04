module Main exposing (main)

import Html exposing (Html, program, div, img, text)
import Html.Attributes exposing (src, style, width)
import DragDrop


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )


type alias Model =
    { dragItem : Maybe DragDrop.DragItem
    , dragDrop : DragDrop.Model
    }


view : Model -> Html Msg
view model =
    div []
        [ viewDiv 0 model.dragItem model.dragDrop.dragItem
        , viewDiv 1 model.dragItem model.dragDrop.dragItem
        , viewDiv 2 model.dragItem model.dragDrop.dragItem
        ]


viewDiv : Int -> Maybe DragDrop.DragItem -> Maybe DragDrop.DragItem -> Html Msg
viewDiv itemId dragItem activeDragItem =
    let
        isActive =
            Just itemId == Maybe.map .dragId activeDragItem

        dropStyle =
            if isActive then
                []
            else
                case dragItem of
                    Just t ->
                        DragDrop.droppable DragDropMsg t

                    Nothing ->
                        []

        divStyle =
            [ style
                [ ( "border", "1px solid black" )
                , ( "padding", "50px" )
                , ( "text-align", "center" )
                , if isActive then
                    ( "background-color", "cyan" )
                  else
                    ( "", "" )
                ]
            ]

        url =
            "https://upload.wikimedia.org/wikipedia/commons/f/f3/Elm_logo.svg"

        children =
            if isActive then
                case dragItem of
                    Just t ->
                        [ img (src url :: width 100 :: DragDrop.draggable DragDropMsg t) []
                        , text (toString t.dragId)
                        ]

                    Nothing ->
                        []
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
                ( dragDrop, dragItem ) =
                    DragDrop.update dragMsg model.dragDrop

                newModel =
                    { model
                        | dragDrop = dragDrop
                        , dragItem = dragItem
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
    { dragItem = Just { dragId = 0, dropId = 0 }
    , dragDrop = DragDrop.init { dragId = 0, dropId = 0 }
    }
