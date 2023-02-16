module Space exposing (Space, color, empty, filled, size)

import Player exposing (Player)


type Space
    = Space Internals


type alias Internals =
    { state : State }


type State
    = Empty
    | Filled Player


color : Space -> String
color (Space space) =
    case space.state of
        Empty ->
            "fill-amber-100 stroke-zinc-500"

        Filled player ->
            if player == Player.one then
                "fill-lime-100 stroke-zinc-500"

            else
                "fill-cyan-100 stroke-zinc-500"


size : Int
size =
    32


empty : Space
empty =
    Space { state = Empty }


filled : Player -> Space
filled player =
    Space { state = Filled player }
