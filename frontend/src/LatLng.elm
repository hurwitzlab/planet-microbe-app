module LatLng exposing (LatLng, decoder, encode, format, formatList, unique)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra



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
    (String.fromFloat lat) ++ "," ++ (String.fromFloat lng)


formatList : List LatLng -> String
formatList locs =
    case locs of
        [ loc ] ->
            format loc

        _ ->
            locs
                |> List.map
                    (\(LatLng (lat, lng)) ->
                        "(" ++ (String.fromFloat lat) ++ "," ++ (String.fromFloat lng) ++ ")"
                    )
                |> String.join ", "


toTuple : LatLng -> (Float, Float)
toTuple (LatLng (lat, lng))  =
    (lat, lng)


fromTuple : (Float, Float) -> LatLng
fromTuple (lat, lng) =
    LatLng (lat, lng)


unique : List LatLng -> List LatLng
unique locs =
    locs
        |> List.map toTuple
        |> List.Extra.unique
        |> List.map fromTuple