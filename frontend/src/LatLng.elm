module LatLng exposing (LatLng, decoder, encode, format, formatList)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Debug exposing (toString)



-- TYPES


type LatLng
    = LatLng (Float, Float)



-- SERIALIZATION


decoder : Decoder LatLng
decoder =
    Decode.map LatLng (Decode.map2 Tuple.pair (Decode.index 1 Decode.float) (Decode.index 0 Decode.float))


encode : LatLng -> Value
encode (LatLng (lat, lng)) =
    Encode.object
        [ ("latitude", Encode.float lat)
        , ("longitude", Encode.float lng)
        ]



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