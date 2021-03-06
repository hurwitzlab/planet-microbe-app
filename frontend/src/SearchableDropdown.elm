module SearchableDropdown exposing (State, Config, init, view)

import Html exposing (Html, text, div, input, table, tbody, tr, td)
import Html.Attributes exposing (class, style, type_, value, placeholder, autofocus)
import Html.Events exposing (onInput, onClick)


type alias State =
    { value : String
    , results : List (String, String)
    , selectedId : Maybe String
    }


type alias Config msg1 msg2 =
    { placeholder : String
    , autofocus : Bool
    , inputMsg : String -> msg1
    , selectMsg : String -> String -> msg2
    }


init : State
init =
    State "" [] Nothing


view : Config msg msg -> State -> Html msg
view config state =
    let
        invOption (id, name) =
            tr [ onClick (config.selectMsg id name) ] [ td [] [ text name ] ]

        resultTable =
            div [ style "overflow-y" "auto", style "max-height" "10em" ]
                [ table [ class "table-condensed table-hover", style "width" "100%" ]
                    [ tbody [] (List.map invOption state.results) ]
                ]
    in
    div []
        [ input [ class "form-control", type_ "text", autofocus config.autofocus, value state.value, placeholder config.placeholder, onInput config.inputMsg ] []
        , if state.value /= "" && state.results /= [] then
            resultTable
          else
            text ""
        ]
