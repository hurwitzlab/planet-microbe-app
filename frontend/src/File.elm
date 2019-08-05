module File exposing (..)

{-| The interface to the File data structure.
-}

import Http
import HttpBuilder
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as Encode
import Config exposing (apiBaseUrl)



-- TYPES --


type alias File =
    { id : Int
    , url : String
    , type_ : String
    , format : String
    , sampleId : Int
    }



-- SERIALIZATION --


fileDecoder : Decoder File
fileDecoder =
    Decode.succeed File
        |> required "file_id" Decode.int
        |> required "url" Decode.string
        |> required "file_type" Decode.string
        |> required "file_format" Decode.string
        |> optional "sample_id" Decode.int 0



-- REQUESTS --


fetchAllBySamples : List Int -> Http.Request (List File)
fetchAllBySamples sampleIds =
    let
        url =
            apiBaseUrl ++ "/samples/files"

        body =
            Encode.object
                [ ( "ids", Encode.string (sampleIds |> List.map String.fromInt |> String.join ",") ) ]
    in
    HttpBuilder.post url
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list fileDecoder))
        |> HttpBuilder.toRequest
