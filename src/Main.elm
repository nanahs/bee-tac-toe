module Main exposing (main)

import Board exposing (Board)
import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Player exposing (Player, next)
import Space exposing (Space)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events


type alias Model =
    { board : Board Space
    , state : GameState
    }


type GameState
    = Turn Player
    | Done Player


stateToPlayer : GameState -> Player
stateToPlayer state =
    case state of
        Turn player ->
            player

        Done player ->
            player


init : ( Model, Cmd Msg )
init =
    let
        board =
            Board.new 3 3
                |> Board.insert ( 0, 0 ) Space.empty
                |> Board.insert ( 0, 1 ) Space.empty
                |> Board.insert ( 0, 2 ) Space.empty
                |> Board.insert ( 1, 0 ) Space.empty
                |> Board.insert ( 1, 1 ) Space.empty
                |> Board.insert ( 1, 2 ) Space.empty
                |> Board.insert ( 2, 0 ) Space.empty
                |> Board.insert ( 2, 1 ) Space.empty
                |> Board.insert ( 2, 2 ) Space.empty
    in
    ( { board = board
      , state = Turn Player.one
      }
    , Cmd.none
    )


type Msg
    = ClickedSpace ( Int, Int ) Player


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedSpace spaceLoc player ->
            ( case Board.get spaceLoc model.board of
                Just space ->
                    if space == Space.empty then
                        { model
                            | board = Board.insert spaceLoc (Space.filled player) model.board
                            , state = Turn (next player)
                        }

                    else
                        model

                Nothing ->
                    model
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Html.div [ Attributes.class "flex flex-col items-center h-full w-full" ]
        [ Html.h1 [ Attributes.class "text-4xl font-bold text-blue-500" ] [ Html.text "Bizz Bazz Buzz" ]
        , Html.div [] [ Html.text (Player.toString (stateToPlayer model.state)) ]
        , Html.div [] [ viewBoard model ]
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    let
        ( height, width ) =
            ( String.fromInt (Board.height model.board * Space.size)
            , String.fromInt (Board.width model.board * Space.size)
            )

        allSpaces =
            List.map Tuple.first (Board.toList model.board)
    in
    allSpaces
        |> List.map (viewSpace model)
        |> Svg.svg
            [ [ "0", "0", height, width ]
                |> String.join " "
                |> Svg.Attributes.viewBox
            , Svg.Attributes.width "100%"
            , Svg.Attributes.height "100%"
            ]


viewSpace : Model -> ( Int, Int ) -> Svg Msg
viewSpace model (( x, y ) as spaceLoc) =
    Svg.rect
        [ Svg.Attributes.height (String.fromInt Space.size)
        , Svg.Attributes.width (String.fromInt Space.size)
        , Svg.Attributes.strokeWidth ".1%"
        , Svg.Attributes.x (String.fromInt (x * Space.size))
        , Svg.Attributes.y (String.fromInt (y * Space.size))
        , model.board
            |> Board.get spaceLoc
            |> Maybe.map Space.color
            |> Maybe.withDefault (Space.color Space.empty)
            |> Svg.Attributes.class
        , Svg.Events.onClick (ClickedSpace spaceLoc (stateToPlayer model.state))
        ]
        []


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , update = update
        , view =
            \model ->
                { title = "elm-template"
                , body = [ view model ]
                }
        , subscriptions = \_ -> Sub.none
        }



-- TEST ONLY


fakeBoard : Svg Msg
fakeBoard =
    Svg.svg
        [ Svg.Attributes.viewBox "0 0 3 3"
        , Svg.Attributes.width "100%"
        , Svg.Attributes.height "100%"
        ]
        [ fakeCell ( 0, 0 )
        , fakeCell ( 0, 1 )
        , fakeCell ( 0, 2 )
        , fakeCell ( 1, 0 )
        , fakeCell ( 1, 1 )
        , fakeCell ( 1, 2 )
        , fakeCell ( 2, 0 )
        , fakeCell ( 2, 1 )
        , fakeCell ( 2, 2 )
        ]


fakeCell : ( Int, Int ) -> Svg Msg
fakeCell ( x, y ) =
    Svg.rect
        [ Svg.Attributes.height "1"
        , Svg.Attributes.width "1"
        , Svg.Attributes.strokeWidth ".1%"
        , Svg.Attributes.x (String.fromInt x)
        , Svg.Attributes.y (String.fromInt y)
        , Svg.Attributes.class "fill-amber-100 stroke-zinc-500"
        ]
        []
