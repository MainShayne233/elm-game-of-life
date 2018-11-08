module Main exposing (CellState(..), Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, img, p, text)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)



---- MODEL ----


type CellState
    = Filled
    | NotFilled


type alias Cell =
    { state : CellState }


type alias Row =
    Array Cell


type alias Board =
    Array Row


type alias Model =
    { board : Board }


rowCount : Int
rowCount =
    20


columnCount : Int
columnCount =
    20


initBoard : Board
initBoard =
    List.repeat rowCount
        (List.repeat columnCount (Cell NotFilled)
            |> Array.fromList
        )
        |> Array.fromList


init : ( Model, Cmd Msg )
init =
    ( { board = initBoard }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | CellClick Int Int Cell
    | Clear


updateClickedCell : Board -> Int -> Int -> Cell -> Board
updateClickedCell board rowIndex columnIndex cell =
    let
        newCellState =
            case cell.state of
                Filled ->
                    NotFilled

                NotFilled ->
                    Filled

        row =
            Array.get rowIndex board
                |> Maybe.withDefault (Array.fromList [])

        cellToUpdate =
            Array.get columnIndex row
                |> Maybe.withDefault (Cell cell.state)
    in
    Array.set rowIndex (Array.set columnIndex { cellToUpdate | state = newCellState } row) board


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CellClick rowIndex columnIndex cell ->
            ( { model | board = updateClickedCell model.board rowIndex columnIndex cell }, Cmd.none )

        Clear ->
            ( { model | board = initBoard }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Game of Life!" ]
        , renderBoard model
        , button [ onClick Clear ] [ text "Clear" ]
        ]


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
    , style "height" "20px"
    , style "width" "20px"
    , style "border-style" "solid"
    , style "border-width" "thin"
    ]


cellColor : Cell -> String
cellColor cell =
    case cell.state of
        Filled ->
            "yellow"

        NotFilled ->
            "white"



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
