module Tree exposing (..)


type Tree a
    = Empty
    | Node a (List (Tree a))


type alias NodeItem =
    { id : Int
    , textColor : String
    }


count : Tree NodeItem -> Int
count tree =
    List.length (flatten tree)


flatten : Tree b -> List b
flatten tree =
    case tree of
        Empty ->
            []

        Node t y ->
            t :: List.concatMap flatten y


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
    let
        newNode =
            Node newNodeItem [ Empty ]
    in
        case tree of
            Empty ->
                newNode

            Node t y ->
                if t.id == position then
                    Node t (newNode :: y)
                else
                    Node t (List.map (insert position newNodeItem) y)


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
        Empty ->
            Empty

        Node nodeItem treeList ->
            Node (func nodeItem) (List.map (map func) treeList)
