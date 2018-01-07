module Tree exposing (..)


type Tree
    = Empty
    | Node NodeItem (List Tree)


type alias NodeItem =
    { id : Int
    , textColor : String
    }


count : Tree -> Int
count tree =
    List.length (flatten tree)


flatten : Tree -> List NodeItem
flatten tree =
    case tree of
        Empty ->
            []

        Node t y ->
            t :: List.concatMap flatten y


insert : Int -> NodeItem -> Tree -> Tree



-- I think... not correct?


insert position newNodeItem tree =
    let
        newNode =
            Node newNodeItem [ Empty ]
    in
        case tree of
            Empty ->
                newNode

            Node t y ->
                Node t (newNode :: y)


update : NodeItem -> Tree -> Tree
update nodeItem tree =
    map
        (\t ->
            if t.id == nodeItem.id then
                nodeItem
            else
                t
        )
        tree


map : (NodeItem -> NodeItem) -> Tree -> Tree
map func tree =
    case tree of
        Empty ->
            Empty

        Node nodeItem treeList ->
            Node (func nodeItem) (List.map (map func) treeList)
