module Main exposing (main)

import Board exposing (Board)
import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Images
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


isDone : GameState -> Bool
isDone state =
    case state of
        Turn _ ->
            False

        Done _ ->
            True


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
    | ClickedRestart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedSpace spaceLoc player ->
            clickedSpace model spaceLoc player

        InputtedBoardSize (Just newSize) ->
            let
                board =
                    createBoard newSize
            in
            ( { model
                | board = board
                , boardSize = newSize
                , state = Turn Player.one
                , winningVariations = winningVariations board
              }
            , Cmd.none
            )

        InputtedBoardSize Nothing ->
            -- ignore
            ( model, Cmd.none )

        ClickedRestart ->
            let
                board =
                    createBoard model.boardSize
            in
            ( { model
                | board = board
                , state = Turn Player.one
              }
            , Cmd.none
            )


clickedSpace : Model -> ( Int, Int ) -> Player -> ( Model, Cmd Msg )
clickedSpace model spaceLoc player =
    if isDone model.state then
        ( model, Cmd.none )

    else
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


view : Model -> Html Msg
view model =
    let
        player =
            stateToPlayer model.state
    in
    Html.div [ Attributes.class "flex flex-col items-center h-full w-full space-y-4" ]
        [ Html.h1 [ Attributes.class "text-4xl font-bold text-blue-500" ] [ Html.text "Bizz Bazz Buzz" ]
        , Html.div [ Attributes.class "flex flex-col space-y-2" ]
            [ Html.div [ Attributes.class "relative" ]
                [ if isDone model.state then
                    winnerOverlay player

                  else
                    Html.text ""
                , Html.div [] [ viewBoard model ]
                ]
            , Html.span [] [ Html.text (Player.toString player ++ "'s turn") ]
            , Html.div [ Attributes.class "flex flex-col" ]
                [ Html.span [] [ Html.text (String.fromInt model.boardSize ++ " in a row to win") ]
                , Html.input
                    [ Attributes.type_ "range"
                    , Attributes.min "1"
                    , Attributes.max "10"
                    , Attributes.step "1"
                    , Events.onInput (InputtedBoardSize << String.toInt)
                    , Attributes.value (String.fromInt model.boardSize)
                    ]
                    []
                ]
            , Html.button
                [ Events.onClick ClickedRestart
                , Attributes.class "border border-slate-200 hover:bg-slate-100 px-4 py-2 rounded-md"
                ]
                [ Html.text "RESTART GAME" ]
            ]
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
    Svg.g
        [ Svg.Attributes.strokeWidth ".1%"
        , Svg.Attributes.class (Space.color Space.empty)
        ]
        [ Svg.rect
            [ Svg.Attributes.height (String.fromInt Space.size)
            , Svg.Attributes.width (String.fromInt Space.size)
            , Svg.Attributes.x (String.fromInt (x * Space.size))
            , Svg.Attributes.y (String.fromInt (y * Space.size))
            , Svg.Events.onClick (ClickedSpace spaceLoc (stateToPlayer model.state))
            ]
            []
        , Svg.svg
            [ Svg.Attributes.height (String.fromInt Space.size)
            , Svg.Attributes.width (String.fromInt Space.size)
            , Svg.Attributes.x (String.fromInt (x * Space.size))
            , Svg.Attributes.y (String.fromInt (y * Space.size))
            ]
            [ model.board
                |> Board.get spaceLoc
                |> Maybe.andThen
                    (\space ->
                        space
                            |> Space.player
                            |> Maybe.map
                                (\player ->
                                    if player == Player.one then
                                        Images.bee

                                    else
                                        Images.honeycomb
                                )
                    )
                |> Maybe.withDefault (Svg.g [] [])
            ]
        ]


winnerOverlay : Player -> Html Msg
winnerOverlay player =
    Html.div
        [ Attributes.class "absolute top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2 font-bold text-2xl whitespace-nowrap"
        ]
        [ Html.text (Player.toString player ++ " WINS!") ]


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , update = update
        , view =
            \model ->
                { title = "BIZZ BAZZ BUZZ"
                , body = [ view model ]
                }
        , subscriptions = \_ -> Sub.none
        }
