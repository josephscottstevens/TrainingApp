module Main exposing (main)

import Html exposing (Html, Attribute, program, div, img, text, input)
import Html.Attributes exposing (attribute, src, style, width, value)
import Html.Events exposing (on, onWithOptions, onClick, onInput)
import Json.Decode as Json


type alias Node =
    { id : Int
    , textColor : String
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
    , selectedId : Maybe Int
    , currentNode : Node
    }


view : Model -> Html Msg
view model =
    div []
        [ img (src url :: width 100 :: draggable -1) []
        , viewSelectedItem model
        , simpleTree "" model.currentNode
        , nodesToHtml model model.currentNode
        ]


viewSelectedItem : Model -> Html Msg
viewSelectedItem model =
    case model.selectedId of
        Just selectedId ->
            let
                selectStyle =
                    style
                        [ ( "float", "right" )
                        , ( "margin-right", "20px" )
                        , ( "margin-top", "20px" )
                        ]

                node =
                    getTreeItemById selectedId model.currentNode

                selectedText =
                    case node of
                        Just t ->
                            input [ value t.textColor, onInput (UpdateTextColor selectedId) ] []

                        Nothing ->
                            text ""
            in
                div [ selectStyle ]
                    [ div [] [ text ("Id: " ++ (toString selectedId)) ]
                    , div [] [ selectedText ]
                    ]

        Nothing ->
            div [] []


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

        isSelected =
            Just itemId == model.selectedId

        node =
            getTreeItemById itemId model.currentNode

        textColor =
            case node of
                Just t ->
                    t.textColor

                Nothing ->
                    ""

        divStyle =
            [ style
                [ ( "border", "1px solid black" )
                , ( "padding", "50px" )
                , ( "text-align", "center" )
                , ( "text-color", textColor )
                , if isActive then
                    ( "background-color", "cyan" )
                  else if isSelected then
                    ( "background-color", "lightyellow" )
                  else
                    ( "", "" )
                ]
            , onClick (SetSelected itemId)
            ]

        children =
            if isActive then
                [ text (toString itemId), img (src url :: width 100 :: draggable itemId) [] ]
            else
                [ text (toString itemId) ]
    in
        div (divStyle ++ droppable itemId) children


type Msg
    = DragStart Int
    | DragEnd
    | DragEnter Int
    | DragLeave Int
    | Drop Int
    | SetSelected Int
    | UpdateTextColor Int String


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
            { model | currentNode = insertNode dropId (countTree model.currentNode + 1) model.currentNode } ! []

        SetSelected selectedId ->
            { model | selectedId = Just selectedId } ! []

        UpdateTextColor itemId textColor ->
            let
                node =
                    getTreeItemById itemId model.currentNode
            in
                model ! []


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
    , selectedId = Nothing
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
                { node | nodes = Nodes [ defaultNode newId ] }

            Nodes t ->
                { node | nodes = Nodes (defaultNode newId :: t) }
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


getTreeItemById : Int -> Node -> Maybe Node
getTreeItemById searchId node =
    if node.id == searchId then
        Just node
    else
        treeToList node
            |> List.filter (\t -> t.id == searchId)
            |> List.head


treeToList : Node -> List Node
treeToList node =
    case node.nodes of
        Empty ->
            [ node ]

        Nodes t ->
            node :: t


simpleTree : String -> Node -> Html Msg
simpleTree format node =
    case node.nodes of
        Empty ->
            viewTree format node.id

        Nodes t ->
            div [] (viewTree format node.id :: List.map (simpleTree (format ++ "--")) t)


defaultNode : Int -> Node
defaultNode id =
    defaultNodeWithChildren id []


defaultNodeWithChildren : Int -> List Node -> Node
defaultNodeWithChildren id nodes =
    { id = id
    , textColor = "black"
    , nodes = Nodes nodes
    }


testNode : Node
testNode =
    defaultNodeWithChildren 0
        [ defaultNode 1
        , defaultNodeWithChildren 2
            [ defaultNode 3
            , defaultNode 4
            ]
        ]
