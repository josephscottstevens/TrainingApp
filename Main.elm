module Main exposing (main)

import Html exposing (Html, Attribute, program, div, img, text, span)
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
    { dragId : Maybe Int
    , dropId : Maybe Int
    , currentNode : Node
    }


view : Model -> Html Msg
view model =
    div []
        [ img (src url :: width 100 :: draggable -1) []
        , simpleTree "" model.currentNode
        , nodesToHtml model testNode
        ]


nodesToHtml : Model -> Node -> Html Msg
nodesToHtml model node =
    case node.nodes of
        Empty ->
            viewDiv model node.id

        Nodes t ->
            div [] (viewDiv model node.id :: List.map (nodesToHtml model) t)


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
                [ img (src url :: width 100 :: draggable itemId) [] ]
            else
                []
    in
        div (divStyle ++ droppable itemId) children


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
            { model
                | currentNode = insertNode dropId (countTree model.currentNode + 1) model.currentNode
            }
                ! []


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
    { dropId = Nothing
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


insertNode : Int -> Int -> Node -> Node
insertNode searchId newId node =
    if node.id == searchId then
        case node.nodes of
            Empty ->
                { node | nodes = Nodes [ { id = newId, nodes = Empty } ] }

            Nodes t ->
                { node | nodes = Nodes ({ id = newId, nodes = Empty } :: t) }
    else
        case node.nodes of
            Empty ->
                node

            Nodes t ->
                { node | nodes = Nodes (List.map (insertNode searchId newId) t) }


countTree : Node -> Int
countTree node =
    List.length (treeIds [] node)


treeIds : List Int -> Node -> List Int
treeIds counts node =
    case node.nodes of
        Empty ->
            node.id :: counts

        Nodes t ->
            node.id :: List.concatMap (treeIds counts) t


viewTree : String -> Int -> Html Msg
viewTree format id =
    div [] [ text (format ++ toString id) ]


simpleTree : String -> Node -> Html Msg
simpleTree format node =
    case node.nodes of
        Empty ->
            viewTree format node.id

        Nodes t ->
            div [] (viewTree format node.id :: List.map (simpleTree (format ++ "--")) t)


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
