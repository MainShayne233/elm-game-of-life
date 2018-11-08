module Main exposing (CellState(..), Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)



---- MODEL ----


type CellState
    = Filled
    | NotFilled


type alias Cell =
    { state : CellState, rowIndex : Int, columnIndex : Int }


type alias Board =
    List Cell


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
    List.range 0 rowCount
        |> List.concatMap
            (\rowIndex ->
                List.range 0 columnCount
                    |> List.map (\columnIndex -> Cell NotFilled rowIndex columnIndex)
            )


init : ( Model, Cmd Msg )
init =
    ( { board = initBoard }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
