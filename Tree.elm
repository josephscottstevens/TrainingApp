module Tree exposing (..)

import Html exposing (Html, div)


type Tree a
    = Element a
    | Node a (List (Tree a))


type alias NodeItem =
    { id : Int
    , textColor : String
    , padding : String
    , backColor : String
    , border : String
    }


count : Tree NodeItem -> Int
count tree =
    List.length (flatten tree)


flatten : Tree b -> List b
flatten tree =
    case tree of
        Element node ->
            [ node ]

        Node t y ->
            t :: List.concatMap flatten y


flattenWithDepth : Int -> Tree b -> List ( b, Int )
flattenWithDepth depth tree =
    case tree of
        Element node ->
            [ ( node, depth ) ]

        Node t y ->
            ( t, depth ) :: List.concatMap (flattenWithDepth (depth + 1)) y


maybeFind : Maybe Int -> Tree NodeItem -> Maybe NodeItem
maybeFind maybeInt tree =
    case maybeInt of
        Just int ->
            map
                (\t ->
                    if t.id == int then
                        Just t
                    else
                        Nothing
                )
                tree
                |> flatten
                |> List.filterMap identity
                |> List.head

        Nothing ->
            Nothing


insert : Int -> NodeItem -> Tree NodeItem -> Tree NodeItem
insert position newNodeItem tree =
    case tree of
        Element node ->
            if node.id == position then
                Node node [ Element newNodeItem ]
            else
                Element node

        Node node y ->
            if node.id == position then
                Node node (Element newNodeItem :: List.map (insert position newNodeItem) y)
            else
                Node node (List.map (insert position newNodeItem) y)


update : NodeItem -> Tree NodeItem -> Tree NodeItem
update nodeItem tree =
    map
        (\t ->
            if t.id == nodeItem.id then
                nodeItem
            else
                t
        )
        tree


map : (a -> b) -> Tree a -> Tree b
map func tree =
    case tree of
        Element node ->
            Element (func node)

        Node nodeItem treeList ->
            Node (func nodeItem) (List.map (map func) treeList)


toHtml : (NodeItem -> Html msg) -> Tree NodeItem -> Html msg
toHtml func tree =
    case tree of
        Element node ->
            func node

        Node node treeList ->
            div [] [ func node, div [] (List.map (toHtml func) treeList) ]
