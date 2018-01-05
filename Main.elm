module Main exposing (main)

import Html exposing (Html, Attribute, program, div, img, text)
import Html.Attributes exposing (attribute, src, style, width)
import Html.Events exposing (on, onWithOptions)
import Json.Decode as Json


type alias Node =
    { id : Int
    , nodes : Nodes
    }


type Nodes
    = Empty
    | Nodes (List Node)


z : Node
z =
    { id = 1, nodes = Nodes [ y ] }


y : Node
y =
    { id = 0, nodes = Empty }


x : List Node
x =
    case y.nodes of
        Empty ->
            []

        Nodes t ->
            t



-- case node.nodes of
--     [] ->
--         div [] [ text (toString node.id) ]
--     first :: rest ->
--         div [] [ text "eh" ]


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )


type alias Model =
    { dragItems : List Int
    , dragId : Maybe Int
    , dropId : Maybe Int
    }


view : Model -> Html Msg
view model =
    let
        len =
            1 + List.length model.dragItems
    in
        div []
            [ img (src url :: width 100 :: (draggable len)) []
            , div [] (List.map (viewDiv model) model.dragItems)
            ]


viewDiv : Model -> Int -> Html Msg
viewDiv model itemId =
    let
        isActive =
            Just itemId == model.dropId

        dropStyle =
            droppable itemId

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
                [ img (src url :: width 100 :: draggable itemId) []
                , text ("activeDragId" ++ toString model.dragId)
                , text ("activeDropId" ++ toString model.dropId)
                ]
            else
                []
    in
        div (divStyle ++ dropStyle) children


type Msg
    = DragStart Int
    | DragEnd
    | DragEnter Int
    | DragLeave Int
    | Drop Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragStart dragId ->
            { model | dragId = Just dragId } ! []

        DragEnd ->
            { model | dragId = Nothing, dropId = Nothing } ! []

        DragEnter dropId ->
            { model | dropId = Just dropId } ! []

        DragLeave dragId ->
            { model | dragId = Just dragId } ! []

        Drop dropId ->
            { model | dragId = Nothing, dragItems = dropId :: model.dragItems } ! []


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
    , dropId = Nothing
    , dragId = Nothing
    }


draggable : Int -> List (Html.Attribute Msg)
draggable dragId =
    [ attribute "draggable" "true"
    , on "dragstart" <| Json.succeed <| DragStart dragId
    , on "dragend" <| Json.succeed <| DragEnd
    , attribute "ondragstart" "event.dataTransfer.setData('text/plain', '');"
    ]


droppable : Int -> List (Attribute Msg)
droppable dropId =
    [ on "dragenter" <| Json.succeed <| DragEnter dropId
    , on "dragleave" <| Json.succeed <| DragLeave dropId
    , onWithOptions "drop" { stopPropagation = True, preventDefault = True } <|
        Json.succeed <|
            Drop dropId
    , attribute "ondragover" "event.stopPropagation(); event.preventDefault();"
    ]


url =
    "https://upload.wikimedia.org/wikipedia/commons/f/f3/Elm_logo.svg"
