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


testNode : Node
testNode =
    { id = 0
    , nodes =
        Nodes
            [ { id = 1, nodes = Empty }
            , { id = 2
              , nodes =
                    Nodes
                        [ { id = 3, nodes = Empty }
                        , { id = 4, nodes = Empty }
                        ]
              }
            ]
    }


divStyle : Attribute msg
divStyle =
    style
        [ ( "width", "100px" )
        , ( "height", "100px" )
        , ( "border-color", "red" )
        , ( "border-width", "1px" )
        , ( "border-style", "solid" )
        , ( "text-align", "center" )
        ]


idToDiv : Int -> Html Msg
idToDiv id =
    div (divStyle :: droppable id) [ text (toString id) ]


nodesToHtml : Node -> Html Msg
nodesToHtml node =
    case node.nodes of
        Empty ->
            idToDiv node.id

        Nodes t ->
            div [] (idToDiv node.id :: List.map nodesToHtml t)


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
            , nodesToHtml testNode
            , div [] (List.map (viewDiv model.dropId model.dragId) model.dragItems)
            ]


viewDiv : Maybe Int -> Maybe Int -> Int -> Html Msg
viewDiv dropId dragId itemId =
    let
        isActive =
            Just itemId == dropId

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
                , text ("activeDragId" ++ toString dragId)
                , text ("activeDropId" ++ toString dropId)
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
    { dragItems = [ 7, 8, 9 ]
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


url : String
url =
    "https://upload.wikimedia.org/wikipedia/commons/f/f3/Elm_logo.svg"
