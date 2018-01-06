module Tree exposing (Tree, NodeItem, testNode, defaultNode, count)


type Tree a
    = Empty
    | Node a (List (Tree a))


type alias NodeItem =
    { id : Int
    , textColor : String
    }


count : Tree NodeItem -> Int
count tree =
    case tree of
        Empty ->
            0

        Node t y ->
            List.length y



-- Temp


defaultNode : Int -> NodeItem
defaultNode id =
    { id = id
    , textColor = "black"
    }


testNode : Tree NodeItem
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
