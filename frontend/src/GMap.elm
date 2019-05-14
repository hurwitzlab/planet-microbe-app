port module GMap exposing (Location, loadMap, setLocation, getLocation, view)

import Html exposing (Html, Attribute, node)
import Json.Encode exposing (Value)


type alias Location =
    { lat : Float
    , lng : Float
    , radius : Float
    }


port loadMap : Value -> Cmd msg


port setLocation : Location -> Cmd msg


port getLocation : (Location -> msg) -> Sub msg


view : List (Attribute msg) -> List (Html msg) -> Html msg
view =
    node "gmap"