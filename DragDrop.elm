module DragDrop
    exposing
        ( Model
        , DragItem
        , init
        , Msg
        , update
          --, getDragId
          --, getDropId
        , draggable
        , droppable
        )

import Html exposing (Attribute)
import Html.Attributes exposing (attribute)
import Html.Events exposing (on, onWithOptions)
import Json.Decode as Json


type alias DragItem =
    { dragId : Int
    , dropId : Int
    }


type State
    = NotDragging
    | Dragging
    | DraggedOver


type alias Model =
    { state : State
    , dragItem : Maybe DragItem
    }


init : DragItem -> Model
init dragItem =
    { state = NotDragging
    , dragItem = Just dragItem
    }


type Msg
    = DragStart DragItem Int
    | DragEnd
    | DragEnter DragItem Int Int
    | DragLeave DragItem Int
    | Drop DragItem Int Int


update : Msg -> Model -> ( Model, Maybe DragItem )
update msg model =
    case msg of
        DragStart dragItem dragId ->
            ( { model | state = Dragging, dragItem = Just { dragItem | dragId = dragId } }, Nothing )

        DragEnd ->
            ( { model | state = NotDragging }, Nothing )

        DragEnter dragItem dragId dropId ->
            ( { model | state = DraggedOver, dragItem = Just { dragItem | dragId = dragId, dropId = dropId } }, Nothing )

        DragLeave dragItem dropId ->
            if dropId == dragItem.dropId then
                ( { model | state = Dragging, dragItem = Just { dragItem | dropId = dropId } }, Nothing )
            else
                ( model, Nothing )

        Drop dragItem dragId dropId ->
            ( { model | state = NotDragging }, Just { dragId = dragId, dropId = dropId } )


draggable : (Msg -> msg) -> DragItem -> List (Attribute msg)
draggable wrap dragItem =
    [ attribute "draggable" "true"
    , on "dragstart" <| Json.succeed <| wrap <| DragStart dragItem dragItem.dragId
    , on "dragend" <| Json.succeed <| wrap <| DragEnd
    , attribute "ondragstart" "event.dataTransfer.setData('text/plain', '');"
    ]


droppable : (Msg -> msg) -> DragItem -> List (Attribute msg)
droppable wrap dragItem =
    [ on "dragenter" <| Json.succeed <| wrap <| DragEnter dragItem dragItem.dragId dragItem.dropId
    , on "dragleave" <| Json.succeed <| wrap <| DragLeave dragItem dragItem.dropId
    , onWithOptions "drop" { stopPropagation = True, preventDefault = True } <|
        Json.succeed <|
            wrap <|
                Drop dragItem dragItem.dragId dragItem.dropId
    , attribute "ondragover" "event.stopPropagation(); event.preventDefault();"
    ]
