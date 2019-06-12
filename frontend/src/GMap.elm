port module GMap exposing (Location, Settings, encodeSettings, loadMap, mapLoaded, removeMap, setLocation, getLocation, changeMapSettings, view)

import Html exposing (Html, Attribute, node)
import Json.Encode as Encode exposing (Value)


type alias Location =
    { lat : Float
    , lng : Float
    , radius : Float
    }


type alias Settings =
    { showDrawingManager : Bool
    , showMarkerClusters : Bool
    , fitBounds: Bool
    , fullscreenControl : Bool
    }


encodeSettings : Settings -> Value
encodeSettings settings =
    Encode.object
        [ ("showDrawingManager", Encode.bool settings.showDrawingManager)
        , ("showMarkerClusters", Encode.bool settings.showMarkerClusters)
        , ("fitBounds", Encode.bool settings.fitBounds)
        , ("fullscreenControl", Encode.bool settings.fullscreenControl)
        ]


port loadMap : Value -> Cmd msg


port mapLoaded : (Bool -> msg) -> Sub msg


port removeMap : String -> Cmd msg


port setLocation : Maybe Location -> Cmd msg


port getLocation : (Maybe Location -> msg) -> Sub msg


port changeMapSettings : Value -> Cmd msg


view : List (Attribute msg) -> List (Html msg) -> Html msg
view =
    node "gmap"