module File exposing (..)

{-| The interface to the File data structure.
-}

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)



-- TYPES


type alias File =
    { id : Int
    , url : String
    , type_ : String
    , format : String
    }



-- SERIALIZATION


fileDecoder : Decoder File
fileDecoder =
    Decode.succeed File
        |> required "file_id" Decode.int
        |> required "url" Decode.string
        |> required "file_type" Decode.string
        |> required "file_format" Decode.string
