module Main exposing (..)

import Html exposing (Html, Attribute, program, div, img, text, input)
import Html.Attributes exposing (attribute, src, style, width, value, id)
import Html.Events exposing (on, onWithOptions, onClick, onInput)
import Tree exposing (Tree(..), NodeItem)
import Json.Decode as Json


type alias Model =
    { dragNode : Maybe NodeItem
    , dropNode : Maybe NodeItem
    , selectedNode : Maybe Int
    , tree : Tree NodeItem
    }


view : Model -> Html Msg
view model =
    div [ id "main" ]
        [ div [ id "controls" ]
            [ img (src url :: width 100 :: draggable (defaultNode -1)) []
            , div (width 100 :: draggable (defaultNode -2)) [ text "Block" ]
            ]
        , div [ id "selected" ]
            [ viewSelectedItem model
            ]
        , div [ id "minimap" ]
            [ div [] (model.tree |> Tree.flattenWithDepth 0 |> (List.map viewMiniTree))
            ]
        , div [ id "body" ]
            [ Tree.toHtml (viewDiv model) model.tree
            ]
        ]


viewSelectedItem : Model -> Html Msg
viewSelectedItem model =
    case Tree.maybeFind model.selectedNode model.tree of
        Just node ->
            let
                defaultInput val event =
                    input [ value val, onInput (event node) ] []
            in
                div []
                    [ div [] [ text ("Id: " ++ (toString node.id)) ]
                    , div [] [ text "Text color: ", defaultInput node.textColor UpdateTextColor ]
                    , div [] [ text "Padding: ", defaultInput node.padding UpdatePadding ]
                    , div [] [ text "Backcolor: ", defaultInput node.backColor UpdateBackColor ]
                    , div [] [ text "Border: ", defaultInput node.border UpdateBorder ]
                    ]

        Nothing ->
            div [] []


viewMiniTree : ( NodeItem, Int ) -> Html Msg
viewMiniTree ( nodeItem, depth ) =
    div [] [ text (String.repeat depth "--" ++ toString (nodeItem.id)) ]


viewDiv : Model -> NodeItem -> Html Msg
viewDiv model nodeItem =
    let
        isActive =
            Just nodeItem == model.dropNode

        isSelected =
            Just nodeItem.id == model.selectedNode

        divStyle =
            [ style
                [ ( "border", nodeItem.border )
                , ( "padding", nodeItem.padding )
                , ( "color", nodeItem.textColor )
                , ( "text-align", "center" )
                , if isActive then
                    ( "background-color", "cyan" )
                  else if isSelected then
                    ( "background-color", "lightyellow" )
                  else
                    ( "background-color", nodeItem.backColor )
                ]
            , onClick (SetSelected nodeItem)
            ]
    in
        div (divStyle ++ droppable nodeItem) [ text (toString nodeItem.id) ]


type Msg
    = DragStart NodeItem
    | DragEnd
    | DragEnter NodeItem
    | DragLeave NodeItem
    | Drop NodeItem
    | SetSelected NodeItem
    | UpdateTextColor NodeItem String
    | UpdatePadding NodeItem String
    | UpdateBackColor NodeItem String
    | UpdateBorder NodeItem String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateNested newItem =
            { model | tree = Tree.update newItem model.tree } ! []
    in
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

            UpdateTextColor nodeItem t ->
                updateNested { nodeItem | textColor = t }

            UpdatePadding nodeItem t ->
                updateNested { nodeItem | padding = t }

            UpdateBackColor nodeItem t ->
                updateNested { nodeItem | backColor = t }

            UpdateBorder nodeItem t ->
                updateNested { nodeItem | border = t }


main : Program Never Model Msg
main =
    program
        { init = emptyModel ! []
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
    , padding = "50px"
    , backColor = "white"
    , border = "1px solid black"
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
