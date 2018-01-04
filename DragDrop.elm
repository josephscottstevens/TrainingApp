module DragDrop exposing (Model, init, Msg, update, draggable, droppable, getDragId, getDropId)

import Html exposing (Attribute)
import Html.Attributes exposing (attribute)
import Html.Events exposing (on, onWithOptions)
import Json.Decode as Json


type alias DragItem =
    { dragId : Int
    , dropId : Int
    }


type Model dragId dropId
    = NotDragging
    | Dragging dragId
    | DraggedOver dragId dropId


init : Model dragId dropId
init =
    NotDragging


type Msg dragId dropId
    = DragStart dragId
    | DragEnd
    | DragEnter dropId
    | DragLeave dropId
    | Drop dropId


update : Msg dragId dropId -> Model dragId dropId -> ( Model dragId dropId, Maybe ( dragId, dropId ) )
update msg model =
    case ( msg, model ) of
        ( DragStart dragId, _ ) ->
            ( Dragging dragId, Nothing )

        ( DragEnd, _ ) ->
            ( NotDragging, Nothing )

        ( DragEnter dropId, Dragging dragId ) ->
            ( DraggedOver dragId dropId, Nothing )

        ( DragEnter dropId, DraggedOver dragId _ ) ->
            ( DraggedOver dragId dropId, Nothing )

        ( DragLeave dropId_, DraggedOver dragId dropId ) ->
            if dropId_ == dropId then
                ( Dragging dragId, Nothing )
            else
                ( model, Nothing )

        ( Drop dropId, Dragging dragId ) ->
            ( NotDragging, Just ( dragId, dropId ) )

        ( Drop dropId, DraggedOver dragId _ ) ->
            ( NotDragging, Just ( dragId, dropId ) )

        _ ->
            ( model, Nothing )


draggable : (Msg dragId dropId -> msg) -> dragId -> List (Attribute msg)
draggable wrap drag =
    [ attribute "draggable" "true"
    , on "dragstart" <| Json.succeed <| wrap <| DragStart drag
    , on "dragend" <| Json.succeed <| wrap <| DragEnd
    , attribute "ondragstart" "event.dataTransfer.setData('text/plain', '');"
    ]


droppable : (Msg dragId dropId -> msg) -> dropId -> List (Attribute msg)
droppable wrap dropId =
    [ on "dragenter" <| Json.succeed <| wrap <| DragEnter dropId
    , on "dragleave" <| Json.succeed <| wrap <| DragLeave dropId
    , onWithOptions "drop" { stopPropagation = True, preventDefault = True } <| Json.succeed <| wrap <| Drop dropId
    , attribute "ondragover" "event.stopPropagation(); event.preventDefault();"
    ]


getDragId : Model dragId dropId -> Maybe dragId
getDragId model =
    case model of
        NotDragging ->
            Nothing

        Dragging dragId ->
            Just dragId

        DraggedOver dragId _ ->
            Just dragId


getDropId : Model dragId dropId -> Maybe dropId
getDropId model =
    case model of
        NotDragging ->
            Nothing

        Dragging _ ->
            Nothing

        DraggedOver _ dropId ->
            Just dropId
