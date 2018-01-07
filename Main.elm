module Main exposing (..)

import Html exposing (Html, Attribute, program, div, img, text, input)
import Html.Attributes exposing (attribute, src, style, width, value)
import Html.Events exposing (on, onWithOptions, onClick, onInput)
import Tree exposing (Tree(..), NodeItem)
import Json.Decode as Json


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )


type alias Model =
    { dragNode : Maybe NodeItem
    , dropNode : Maybe NodeItem
    , selectedNode : Maybe Int
    , tree : Tree NodeItem
    }


view : Model -> Html Msg
view model =
    div []
        [ img (src url :: width 100 :: draggable (defaultNode -1)) []
        , viewSelectedItem model
        , viewMiniTree "" model.tree
        , viewDiv model model.tree
        ]


viewSelectedItem : Model -> Html Msg
viewSelectedItem model =
    case Tree.maybeFind model.selectedNode model.tree of
        Just selectedNode ->
            let
                selectStyle =
                    style
                        [ ( "float", "right" )
                        , ( "margin-right", "20px" )
                        , ( "margin-top", "20px" )
                        ]

                selectedText =
                    input [ value selectedNode.textColor, onInput (UpdateTextColor selectedNode) ] []
            in
                div [ selectStyle ]
                    [ div [] [ text ("Id: " ++ (toString selectedNode.id)) ]
                    , div [] [ text "Text color: ", selectedText ]
                    ]

        Nothing ->
            div [] []


viewMiniTree : String -> Tree NodeItem -> Html Msg
viewMiniTree dashes tree =
    case tree of
        Element node ->
            div [] []

        Node nodeItem nodeList ->
            let
                newDiv =
                    div [] [ text (dashes ++ toString (nodeItem.id)) ]
            in
                div [] (newDiv :: List.map (viewMiniTree (dashes ++ "--")) nodeList)


viewDiv : Model -> Tree NodeItem -> Html Msg
viewDiv model tree =
    case tree of
        Element node ->
            div [] []

        Node nodeItem nodeList ->
            let
                isActive =
                    Just nodeItem == model.dropNode

                isSelected =
                    Just nodeItem.id == model.selectedNode

                divStyle =
                    [ style
                        [ ( "border", "1px solid black" )
                        , ( "padding", "50px" )
                        , ( "color", nodeItem.textColor )
                        , ( "text-align", "center" )
                        , if isActive then
                            ( "background-color", "cyan" )
                          else if isSelected then
                            ( "background-color", "lightyellow" )
                          else
                            ( "", "" )
                        ]
                    , onClick (SetSelected nodeItem)
                    ]

                children =
                    if isActive then
                        [ text (toString nodeItem.id), img (src url :: width 100 :: draggable nodeItem) [] ]
                    else
                        [ text (toString nodeItem.id) ]

                newDiv =
                    div (divStyle ++ droppable nodeItem) children
            in
                div [] (newDiv :: List.map (viewDiv model) nodeList)


type Msg
    = DragStart NodeItem
    | DragEnd
    | DragEnter NodeItem
    | DragLeave NodeItem
    | Drop NodeItem
    | SetSelected NodeItem
    | UpdateTextColor NodeItem String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragStart dragNode ->
            { model | dragNode = Just dragNode } ! []

        DragEnd ->
            { model | dragNode = Nothing, dropNode = Nothing } ! []

        DragEnter dropNode ->
            { model | dropNode = Just dropNode } ! []

        DragLeave dragNode ->
            { model | dragNode = Just dragNode } ! []

        Drop nodeItem ->
            let
                count =
                    1 + Tree.count model.tree
            in
                { model | tree = Tree.insert nodeItem.id (defaultNode count) model.tree } ! []

        SetSelected nodeItem ->
            { model | selectedNode = Just nodeItem.id } ! []

        UpdateTextColor nodeItem textColor ->
            let
                tree =
                    Tree.update { nodeItem | textColor = textColor } model.tree
            in
                { model | tree = tree } ! []


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
    { dropNode = Nothing
    , dragNode = Nothing
    , selectedNode = Nothing
    , tree = testNode
    }


draggable : NodeItem -> List (Html.Attribute Msg)
draggable dragNode =
    [ attribute "draggable" "true"
    , on "dragstart" <| Json.succeed <| DragStart dragNode
    , on "dragend" <| Json.succeed <| DragEnd
    , attribute "ondragstart" "event.dataTransfer.setData('text/plain', '');"
    ]


droppable : NodeItem -> List (Attribute Msg)
droppable dropNode =
    [ on "dragenter" <| Json.succeed <| DragEnter dropNode
    , on "dragleave" <| Json.succeed <| DragLeave dropNode
    , onWithOptions "drop" { stopPropagation = True, preventDefault = True } <|
        Json.succeed <|
            Drop dropNode
    , attribute "ondragover" "event.stopPropagation(); event.preventDefault();"
    ]


url : String
url =
    "https://upload.wikimedia.org/wikipedia/commons/f/f3/Elm_logo.svg"


defaultNode : Int -> NodeItem
defaultNode id =
    { id = id
    , textColor = "black"
    }


testNode : Tree NodeItem
testNode =
    Node (defaultNode 0)
        [ Node (defaultNode 1)
            [ Element (defaultNode 2)
            ]
        , Element (defaultNode 3)
        , Element (defaultNode 4)
        ]
