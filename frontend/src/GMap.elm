port module GMap exposing (Location, loadMap, mapLoaded, removeMap, setLocation, getLocation, view)

import Html exposing (Html, Attribute, node)
import Json.Encode exposing (Value)


type alias Location =
    { lat : Float
    , lng : Float
    , radius : Float
    }


port loadMap : Value -> Cmd msg


port mapLoaded : (Bool -> msg) -> Sub msg


port removeMap : String -> Cmd msg


port setLocation : Maybe Location -> Cmd msg


port getLocation : (Maybe Location -> msg) -> Sub msg


view : List (Attribute msg) -> List (Html msg) -> Html msg
view =
    node "gmap"