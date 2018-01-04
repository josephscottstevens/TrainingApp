module DragDrop
    exposing
        ( Model
        , DragItem
        , init
        , Msg
        , update
        , getDragId
        , getDropId
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
    , dragId : Int
    , dropId : Int
    }


init : Model
init =
    { state = NotDragging
    , dragId = 0
    , dropId = 0
    }


type Msg
    = DragStart DragItem
    | DragEnd
    | DragEnter DragItem
    | DragLeave DragItem
    | Drop DragItem


update : Msg -> Model -> ( Model, Maybe DragItem )
update msg model =
    case msg of
        DragStart t ->
            ( { model | state = Dragging, dragId = t.dragId }, Nothing )

        DragEnd ->
            ( { model | state = NotDragging }, Nothing )

        DragEnter t ->
            ( { model | state = DraggedOver, dragId = t.dragId, dropId = t.dropId }, Nothing )

        DragLeave t ->
            if t.dropId == model.dropId then
                ( { model | state = Dragging, dropId = t.dropId }, Nothing )
            else
                ( model, Nothing )

        Drop t ->
            ( { model | state = NotDragging }, Just { dragId = t.dragId, dropId = t.dropId } )


draggable : (Msg -> msg) -> DragItem -> List (Attribute msg)
draggable wrap drag =
    [ attribute "draggable" "true"
    , on "dragstart" <| Json.succeed <| wrap <| DragStart drag
    , on "dragend" <| Json.succeed <| wrap <| DragEnd
    , attribute "ondragstart" "event.dataTransfer.setData('text/plain', '');"
    ]


droppable : (Msg -> msg) -> DragItem -> List (Attribute msg)
droppable wrap dropId =
    [ on "dragenter" <| Json.succeed <| wrap <| DragEnter dropId
    , on "dragleave" <| Json.succeed <| wrap <| DragLeave dropId
    , onWithOptions "drop" { stopPropagation = True, preventDefault = True } <| Json.succeed <| wrap <| Drop dropId
    , attribute "ondragover" "event.stopPropagation(); event.preventDefault();"
    ]


getDragId : Model -> Maybe Int
getDragId model =
    case model.state of
        NotDragging ->
            Nothing

        Dragging ->
            Just model.dragId

        DraggedOver ->
            Just model.dragId


getDropId : Model -> Maybe Int
getDropId model =
    case model.state of
        NotDragging ->
            Nothing

        Dragging ->
            Nothing

        DraggedOver ->
            Just model.dropId
