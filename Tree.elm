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


insert : Int -> Tree -> Tree
insert id tree =
    let
        newNode =
            Node (defaultNode id) [ Empty ]
    in
        case tree of
            Empty ->
                newNode

            Node t y ->
                Node t (newNode :: y)


updateTextColor : Int -> String -> Tree -> Tree
updateTextColor id newColor tree =
    map
        (\t ->
            if t.id == id then
                { t | textColor = newColor }
            else
                t
        )
        tree


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



-- Temp


defaultNode : Int -> NodeItem
defaultNode id =
    { id = id
    , textColor = "black"
    }


testNode : Tree
testNode =
    Node (defaultNode 0)
        [ Node (defaultNode 1)
            [ Node (defaultNode 1) [ Empty ]
            , Node (defaultNode 2) [ Empty ]
            , Node (defaultNode 3) [ Empty ]
            ]
        , Node (defaultNode 4) [ Empty ]
        , Node (defaultNode 5) [ Empty ]
        ]
