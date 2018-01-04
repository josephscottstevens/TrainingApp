module Main exposing (main)

import Html exposing (Html, Attribute, program, div, img, text)
import Html.Attributes exposing (attribute, src, style, width)
import Html.Events exposing (on, onWithOptions)
import Json.Decode as Json


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )


type alias Model =
    { dragItems : List Int
    , activeItem : Maybe Int
    }


view : Model -> Html Msg
view model =
    div []
        [ img (src url :: width 100 :: (draggable 3)) []
        , div [] (List.map (viewDiv model.activeItem) model.dragItems)
        ]


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
                        droppable t itemId

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

        children =
            if isActive then
                case activeDragItem of
                    Just t ->
                        [ img (src url :: width 100 :: draggable t) []
                        , text ("activeId" ++ toString activeDragItem)
                        ]

                    Nothing ->
                        []
            else
                []
    in
        div (divStyle ++ dropStyle) children


type Msg
    = DragStart Int
    | DragEnd
    | DragEnter Int Int
    | DragLeave Int
    | Drop Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragStart dragId ->
            { model | activeItem = Just dragId } ! []

        DragEnd ->
            { model | activeItem = Nothing } ! []

        DragEnter dragId dropId ->
            { model | activeItem = Just dragId } ! []

        DragLeave dragId ->
            { model | activeItem = Just dragId } ! []

        Drop dragId dropId ->
            { model | activeItem = Just dragId } ! []


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
    , activeItem = Nothing
    }


draggable : Int -> List (Html.Attribute Msg)
draggable dragId =
    [ attribute "draggable" "true"
    , on "dragstart" <| Json.succeed <| DragStart dragId
    , on "dragend" <| Json.succeed <| DragEnd
    , attribute "ondragstart" "event.dataTransfer.setData('text/plain', '');"
    ]


droppable : Int -> Int -> List (Attribute Msg)
droppable dragId dropId =
    [ on "dragenter" <| Json.succeed <| DragEnter dragId dropId
    , on "dragleave" <| Json.succeed <| DragLeave dropId
    , onWithOptions "drop" { stopPropagation = True, preventDefault = True } <|
        Json.succeed <|
            Drop dragId dropId
    , attribute "ondragover" "event.stopPropagation(); event.preventDefault();"
    ]


url =
    "https://upload.wikimedia.org/wikipedia/commons/f/f3/Elm_logo.svg"
