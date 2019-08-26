module SortableTable exposing (..)

import Html exposing (Html, Attribute, text, table, thead, tbody, tfoot)


type alias State =
    { sortCol : Int
    , sortDir : Direction
    }


initialState : State
initialState =
    { sortCol = 1
    , sortDir = ASC
    }


type alias Config msg =
    { tableAttrs : List (Attribute msg)
    }


type Direction
    = ASC
    | DESC


view : Config msg -> State -> List (Html msg) -> List (Html msg) -> List (Html msg) -> Html msg
view { tableAttrs } state theadNodes tbodyNodes tfootNodes =
    table tableAttrs
        [ thead [] theadNodes
        , tbody [] tbodyNodes
        , tfoot [] tfootNodes
        ]


toggleDirection : Direction -> Direction
toggleDirection direction =
    if direction == ASC then
        DESC
    else
        ASC


directionToInt : Direction -> Int
directionToInt direction =
    if direction == ASC then
        1
    else
        -1
