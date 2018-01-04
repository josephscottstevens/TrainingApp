module DragDrop
    exposing
        ( Model
        , init
        , Msg
        , update
        , draggable
        , droppable
        )

import Html exposing (Attribute)
import Html.Attributes exposing (attribute)
import Html.Events exposing (on, onWithOptions)
import Json.Decode as Json


type Model
    = NotDragging (Maybe Int)
    | Dragging (Maybe Int)
    | DraggedOver (Maybe Int)


init : Model
init =
    NotDragging Nothing


type Msg
    = DragStart Int
    | DragEnd
    | DragEnter Int Int
    | DragLeave Int
    | Drop Int Int


update : Msg -> Model -> Maybe Int -> ( Model, Maybe Int )
update msg model dragItem =
    case msg of
        DragStart dragId ->
            ( Dragging (Just dragId), Just dragId )

        DragEnd ->
            ( NotDragging Nothing, Nothing )

        DragEnter dragId dropId ->
            ( DraggedOver Nothing, Nothing )

        DragLeave dropId ->
            ( NotDragging Nothing, Nothing )

        -- if dropId == dragItem.dropId then
        --     ( Dragging, Nothing )
        -- else
        --     ( model, Nothing )
        Drop dragId dropId ->
            -- ( NotDragging, Just { dragId = dragId, dropId = dropId } )
            ( NotDragging Nothing, Nothing )


draggable : (Msg -> msg) -> Int -> List (Attribute msg)
draggable wrap dragId =
    [ attribute "draggable" "true"
    , on "dragstart" <| Json.succeed <| wrap <| DragStart dragId
    , on "dragend" <| Json.succeed <| wrap <| DragEnd
    , attribute "ondragstart" "event.dataTransfer.setData('text/plain', '');"
    ]


droppable : (Msg -> msg) -> Int -> Int -> List (Attribute msg)
droppable wrap dragId dropId =
    [ on "dragenter" <| Json.succeed <| wrap <| DragEnter dragId dropId
    , on "dragleave" <| Json.succeed <| wrap <| DragLeave dropId
    , onWithOptions "drop" { stopPropagation = True, preventDefault = True } <|
        Json.succeed <|
            wrap <|
                Drop dragId dropId
    , attribute "ondragover" "event.stopPropagation(); event.preventDefault();"
    ]
