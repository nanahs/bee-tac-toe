module Main exposing (main)

import Board exposing (Board)
import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Player exposing (Player, next)
import Space exposing (Space)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events


type alias Model =
    { board : Board Space
    , state : GameState
    , boardSize : Int
    , winningVariations : List (List ( Int, Int ))
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
            createBoard 3
    in
    ( { board = board
      , state = Turn Player.one
      , boardSize = 3
      , winningVariations = winningVariations board
      }
    , Cmd.none
    )


createBoard : Int -> Board Space
createBoard boardSize =
    List.range 0 (boardSize - 1)
        |> List.map
            (\h ->
                List.range 0 (boardSize - 1)
                    |> List.map (\w -> ( h, w ))
            )
        |> List.concat
        |> List.foldl (\tuple -> Board.insert tuple Space.empty) (Board.new boardSize boardSize)


type Msg
    = ClickedSpace ( Int, Int ) Player
    | InputtedBoardSize (Maybe Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedSpace spaceLoc player ->
            ( case updateBoard spaceLoc player model.board of
                Just board ->
                    -- legal move
                    if didPlayerWin board model.winningVariations player then
                        { model
                            | board = board
                            , state = Done player
                        }

                    else
                        { model
                            | board = board
                            , state = Turn (next player)
                        }

                Nothing ->
                    -- illegal move
                    model
            , Cmd.none
            )

        InputtedBoardSize (Just newSize) ->
            let
                board =
                    createBoard newSize
            in
            ( { model
                | board = board
                , boardSize = newSize
                , winningVariations = winningVariations board
              }
            , Cmd.none
            )

        InputtedBoardSize Nothing ->
            -- ignore
            ( model, Cmd.none )


updateBoard : ( Int, Int ) -> Player -> Board Space -> Maybe (Board Space)
updateBoard spaceLoc player board =
    Board.get spaceLoc board
        |> Maybe.andThen
            (\space ->
                if space == Space.empty then
                    Just <| Board.insert spaceLoc (Space.filled player) board

                else
                    Nothing
            )


calcWinner : Board Space -> Player
calcWinner board =
    Board.toList board
        |> (\_ -> Player.one)


winningVariations : Board Space -> List (List ( Int, Int ))
winningVariations board =
    -- interesting notes
    -- boardSize * 2 + 2 = number of possible winning iterations
    -- example for a board size of 3
    --
    -- (0,0)(1,0)(2,0)
    -- (0,1)(1,1)(2,1)
    -- (0,2)(1,2)(2,2)
    -- (0,0)(0,1)(0,2)
    -- (1,0)(1,1)(1,2)
    -- (2,0)(2,1)(2,2)
    -- (0,0)(1,1)(2,2)
    -- (0,2)(1,1)(2,0)
    let
        boardSize =
            Board.height board - 1

        winningRows =
            List.range 0 boardSize |> List.map (\h -> List.range 0 boardSize |> List.map (\w -> ( w, h )))

        winningColumns =
            List.range 0 boardSize |> List.map (\h -> List.range 0 boardSize |> List.map (\w -> ( h, w )))

        downRightDiagWin =
            List.range 0 boardSize |> List.map (\h -> List.range h h |> List.map (\w -> ( h, w ))) |> List.concat

        upRightDiagWin =
            List.range 0 boardSize |> List.map (\h -> List.range (boardSize - h) (boardSize - h) |> List.map (\w -> ( h, w ))) |> List.concat
    in
    [ downRightDiagWin, upRightDiagWin ]
        |> List.append
            (List.concat
                [ winningRows
                , winningColumns
                ]
            )


playerLocs : Board Space -> Player -> List ( Int, Int )
playerLocs board player =
    board
        |> Board.toList
        |> List.filterMap
            (\( spaceLoc, space ) ->
                if Space.player space == Just player then
                    Just spaceLoc

                else
                    Nothing
            )


didPlayerWin : Board Space -> List (List ( Int, Int )) -> Player -> Bool
didPlayerWin board winningVars player =
    let
        oneLocs =
            playerLocs board player
    in
    winningVars
        |> List.map
            (\winningSpaces ->
                List.foldl
                    (\winningSpace hasWinningLocs ->
                        hasWinningLocs && List.member winningSpace oneLocs
                    )
                    True
                    winningSpaces
            )
        |> List.any identity


playerTwoWinner : Model -> Bool
playerTwoWinner model =
    let
        twoLocs =
            playerLocs model.board Player.two
    in
    model.winningVariations
        |> List.map
            (\winningSpaces ->
                List.foldl
                    (\winningSpace hasWinningLocs ->
                        hasWinningLocs && List.member winningSpace twoLocs
                    )
                    True
                    winningSpaces
            )
        |> List.any identity


view : Model -> Html Msg
view model =
    Html.div [ Attributes.class "flex flex-col items-center h-full w-full" ]
        [ Html.h1 [ Attributes.class "text-4xl font-bold text-blue-500" ] [ Html.text "Bizz Bazz Buzz" ]
        , Html.input
            [ Attributes.type_ "range"
            , Attributes.min "0"
            , Attributes.max "10"
            , Attributes.step "1"
            , Events.onInput (InputtedBoardSize << String.toInt)
            , Attributes.value (String.fromInt model.boardSize)
            ]
            []
        , case model.state of
            Turn player ->
                Html.div []
                    [ Html.div [] [ Html.text (Player.toString (stateToPlayer model.state)) ]
                    , Html.div [] [ viewBoard model ]
                    ]

            Done player ->
                Html.div [] [ Html.text "WINNER" ]
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    let
        ( boardHieght, boardWidth ) =
            ( String.fromInt (Board.height model.board * Space.size)
            , String.fromInt (Board.width model.board * Space.size)
            )

        allSpaces =
            List.map Tuple.first (Board.toList model.board)
    in
    allSpaces
        |> List.map (viewSpace model)
        |> Svg.svg
            [ [ "0", "0", boardHieght, boardWidth ]
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
