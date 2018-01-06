module Tree exposing (..)


type Tree
    = Empty
    | Node NodeItem (List Tree)


type alias NodeItem =
    { id : Int
    , textColor : String
    }


flatten : Tree -> List NodeItem
flatten tree =
    case tree of
        Empty ->
            []

        Node t y ->
            t :: List.concatMap flatten y



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
