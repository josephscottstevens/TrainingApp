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


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )


type alias Model =
    { dragItems : List Int
    , dragId : Maybe Int
    , dropId : Maybe Int
    , currentNode : Node
    }


view : Model -> Html Msg
view model =
    let
        len =
            --1 + List.length model.dragItems
            8
    in
        div []
            [ img (src url :: width 100 :: (draggable len)) []
            , nodesToHtml model testNode
            , div [] (List.map (viewDiv model) model.dragItems)
            ]


idToDiv : Model -> Int -> Html Msg
idToDiv model id =
    div (divStyle :: droppable id) [ text (toString id) ]


nodesToHtml : Model -> Node -> Html Msg
nodesToHtml model node =
    case node.nodes of
        Empty ->
            (idToDiv model) node.id

        Nodes t ->
            div [] ((idToDiv model) node.id :: List.map (nodesToHtml model) t)


viewDiv : Model -> Int -> Html Msg
viewDiv model itemId =
    let
        isActive =
            Just itemId == model.dropId

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
        div (divStyle ++ (droppable itemId)) children


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
    , currentNode = testNode
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
