module LatLng exposing (LatLng, decoder, format, formatList)

import Json.Decode as Decode exposing (Decoder)
import Debug exposing (toString)



-- TYPES


type LatLng
    = LatLng (Float, Float)



-- CREATE


decoder : Decoder LatLng
decoder =
    Decode.map LatLng (Decode.map2 Tuple.pair (Decode.index 1 Decode.float) (Decode.index 0 Decode.float))


-- TRANSFORM

format : LatLng -> String
format (LatLng (lat, lng)) =
    (toString lat) ++ "," ++ (toString lng)


formatList : List LatLng -> String
formatList locs =
    case locs of
        [ loc ] ->
            format loc

        _ ->
            locs |> List.map (\(LatLng (lng, lat)) -> "(" ++ (toString lat) ++ "," ++ (toString lng) ++ ")" ) |> String.join ", "