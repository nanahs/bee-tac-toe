module Board exposing (Board, get, height, insert, new, toList, width)

import Dict exposing (Dict)


type Board a
    = Board (Internals a)


type alias Internals a =
    { height : Int
    , width : Int
    , spaces : Dict ( Int, Int ) a
    }


new : Int -> Int -> Board a
new height_ width_ =
    Board
        { height = height_
        , width = width_
        , spaces = Dict.empty
        }


height : Board a -> Int
height (Board board) =
    board.height


width : Board a -> Int
width (Board board) =
    board.width



--


get : ( Int, Int ) -> Board a -> Maybe a
get location (Board board) =
    Dict.get location board.spaces


insert : ( Int, Int ) -> a -> Board a -> Board a
insert spaceLoc val (Board board) =
    Board { board | spaces = Dict.insert spaceLoc val board.spaces }


toList : Board a -> List ( ( Int, Int ), a )
toList (Board board) =
    Dict.toList board.spaces
