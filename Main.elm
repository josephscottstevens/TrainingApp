module Main exposing (main)

import Html exposing (Html, program, div, img, text)
import Html.Attributes exposing (src, style, width)
import DragDrop


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )


type alias Model =
    { dragItems : List Int
    , activeItemId : Maybe Int
    , dragDrop : DragDrop.Model
    }


view : Model -> Html Msg
view model =
    div [] (List.map (viewDiv model.activeItemId) model.dragItems)


viewDiv : Maybe Int -> Int -> Html Msg
viewDiv activeDragItem itemId =
    let
        isActive =
            Just itemId == activeDragItem

        dropStyle =
            if isActive then
                []
            else
                case activeDragItem of
                    Just t ->
                        -- TODO, might be backwards?
                        DragDrop.droppable DragDropMsg t itemId

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
                case activeDragItem of
                    Just t ->
                        [ img (src url :: width 100 :: DragDrop.draggable DragDropMsg t) []
                        , text ("activeId" ++ toString activeDragItem)
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
                ( dragDrop, activeItemId ) =
                    DragDrop.update dragMsg model.dragDrop model.activeItemId

                newModel =
                    { model
                        | dragDrop = dragDrop
                        , activeItemId = activeItemId
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
    { dragItems = [ 0, 1, 2 ]
    , activeItemId = Just 0
    , dragDrop = DragDrop.init
    }
