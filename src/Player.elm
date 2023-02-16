module Player exposing (Player, next, one, toString, two)


type Player
    = Player1
    | Player2


next : Player -> Player
next player =
    case player of
        Player1 ->
            Player2

        Player2 ->
            Player1


toString : Player -> String
toString player =
    case player of
        Player1 ->
            "Player 1"

        Player2 ->
            "Player 2"


one : Player
one =
    Player1


two : Player
two =
    Player2
