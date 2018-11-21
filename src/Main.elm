module Main exposing (CellState(..), Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onAnimationFrame)
import Html exposing (Html, button, div, h1, img, p, text)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import Time



---- MODEL ----


type CellState
    = Filled
    | NotFilled


type Msg
    = NoOp
    | CellClick Int Int Cell
    | Clear
    | SetExecutionState ExecutionState
    | HandleTick Time.Posix


type ExecutionState
    = Running
    | NotRunning


type alias Cell =
    { state : CellState }


type alias Row =
    Array Cell


type alias Board =
    Array Row


type alias Model =
    { board : Board, executionState : ExecutionState }


rowCount : Int
rowCount =
    40


columnCount : Int
columnCount =
    40


initCell : Cell
initCell =
    Cell NotFilled


initRow : Row
initRow =
    Array.initialize columnCount (always initCell)


blankBoard : Board
blankBoard =
    Array.initialize rowCount (always initRow)


initBoard : Board
initBoard =
    blankBoard
        |> fillCell 20 20
        |> fillCell 20 21
        |> fillCell 20 22
        |> fillCell 19 22
        |> fillCell 18 21


init : ( Model, Cmd Msg )
init =
    ( { board = initBoard, executionState = NotRunning }, Cmd.none )



---- UPDATE ----


fillCell : Int -> Int -> Board -> Board
fillCell =
    setCell (Cell Filled)


clearCell : Int -> Int -> Board -> Board
clearCell =
    setCell (Cell NotFilled)


setCell : Cell -> Int -> Int -> Board -> Board
setCell cell rowIndex columnIndex board =
    let
        updatedRow =
            Array.get rowIndex board
                |> Maybe.withDefault (Array.fromList [])
                |> Array.set columnIndex cell
    in
    Array.set rowIndex updatedRow board


amountOfFilledNeighbors : Board -> Int -> Int -> Int
amountOfFilledNeighbors board rowIndex columnIndex =
    List.foldl
        (\cell acc ->
            case cell.state of
                Filled ->
                    acc + 1

                NotFilled ->
                    acc
        )
        0
        [ getCell board (rowIndex - 1) (columnIndex - 1)
        , getCell board rowIndex (columnIndex - 1)
        , getCell board (rowIndex + 1) (columnIndex - 1)
        , getCell board (rowIndex - 1) columnIndex
        , getCell board (rowIndex + 1) columnIndex
        , getCell board (rowIndex - 1) (columnIndex + 1)
        , getCell board rowIndex (columnIndex + 1)
        , getCell board (rowIndex + 1) (columnIndex + 1)
        ]


determineCellState : Board -> Int -> Int -> Cell -> CellState
determineCellState board rowIndex columnIndex cell =
    case ( cell.state, amountOfFilledNeighbors board rowIndex columnIndex ) of
        ( Filled, 2 ) ->
            Filled

        ( _, 3 ) ->
            Filled

        _ ->
            NotFilled


inverseCellState : CellState -> CellState
inverseCellState cellState =
    case cellState of
        Filled ->
            NotFilled

        NotFilled ->
            Filled


updateClickedCell : Board -> Int -> Int -> Cell -> Board
updateClickedCell board rowIndex columnIndex cell =
    let
        row =
            Array.get rowIndex board
                |> Maybe.withDefault (Array.fromList [])

        cellToUpdate =
            Array.get columnIndex row
                |> Maybe.withDefault (Cell cell.state)

        newCellState =
            inverseCellState cellToUpdate.state
    in
    Array.set rowIndex (Array.set columnIndex { cellToUpdate | state = newCellState } row) board


getCell : Board -> Int -> Int -> Cell
getCell board rowIndex columnIndex =
    Array.get rowIndex board
        |> Maybe.withDefault (Array.fromList [])
        |> Array.get columnIndex
        |> Maybe.withDefault (Cell NotFilled)


updateCell : Board -> Int -> Int -> Cell
updateCell board rowIndex columnIndex =
    let
        cell =
            getCell board rowIndex columnIndex

        newState =
            determineCellState board rowIndex columnIndex cell
    in
    { cell | state = newState }


updateBoard : Board -> Board
updateBoard board =
    List.range 0 (columnCount - 1)
        |> List.map
            (\rowIndex ->
                List.range 0 (rowCount - 1)
                    |> List.map (\columnIndex -> updateCell board rowIndex columnIndex)
                    |> Array.fromList
            )
        |> Array.fromList


handleRunningExecution : Model -> Model
handleRunningExecution model =
    let
        updatedBoard =
            updateBoard model.board
    in
    { model | board = updatedBoard }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CellClick rowIndex columnIndex cell ->
            ( { model | board = updateClickedCell model.board rowIndex columnIndex cell }, Cmd.none )

        Clear ->
            ( { model | board = blankBoard }, Cmd.none )

        SetExecutionState executionState ->
            ( { model | executionState = executionState }, Cmd.none )

        HandleTick delta ->
            case model.executionState of
                NotRunning ->
                    ( model, Cmd.none )

                Running ->
                    ( handleRunningExecution model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Game of Life!" ]
        , renderBoard model
        , renderExecutionControlButton model
        , button [ onClick Clear ] [ text "Clear" ]
        ]


renderExecutionControlButton : Model -> Html Msg
renderExecutionControlButton { executionState } =
    case executionState of
        Running ->
            button [ onClick (SetExecutionState NotRunning) ] [ text "Stop" ]

        NotRunning ->
            button [ onClick (SetExecutionState Running) ] [ text "Start" ]


renderCell : Int -> Int -> Cell -> Html Msg
renderCell rowIndex columnIndex cell =
    div (List.append (cellStyles rowIndex columnIndex cell) [ onClick (CellClick rowIndex columnIndex cell) ]) []


renderRow : Int -> Row -> Html Msg
renderRow rowIndex row =
    div (rowStyles rowIndex) (Array.toList (Array.indexedMap (\columnIndex cell -> renderCell rowIndex columnIndex cell) row))


renderBoard : Model -> Html Msg
renderBoard { board } =
    div boardStyles (Array.toList (Array.indexedMap renderRow board))



---- STYLES ----


boardStyles =
    [ style "display" "flex", style "flex-direction" "column", style "align-items" "center" ]


rowStyles rowIndex =
    [ style "display" "flex" ]


cellStyles rowIndex columNindex cell =
    [ style "background-color" (cellColor cell)
    , style "height" "15px"
    , style "width" "15px"
    , style "border-style" "solid"
    , style "border-width" "thin"
    ]


cellColor : Cell -> String
cellColor cell =
    case cell.state of
        Filled ->
            "yellow"

        NotFilled ->
            "grey"



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 100 HandleTick


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
