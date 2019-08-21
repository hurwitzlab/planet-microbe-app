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


type alias FileFormat =
    { id : Int
    , name : String
    , description : String
    , extensions : List String
    , fileCount : Int
    }


type alias FileType =
    { id : Int
    , name : String
    , description : String
    , fileCount : Int
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


fileFormatDecoder : Decoder FileFormat
fileFormatDecoder =
    Decode.succeed FileFormat
        |> required "file_format_id" Decode.int
        |> required "name" Decode.string
        |> optional "description" Decode.string ""
        |> optional "extensions" (Decode.list Decode.string) []
        |> required "file_count" Decode.int


fileTypeDecoder : Decoder FileType
fileTypeDecoder =
    Decode.succeed FileType
        |> required "file_type_id" Decode.int
        |> required "name" Decode.string
        |> optional "description" Decode.string ""
        |> required "file_count" Decode.int



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


fetchFormats : Http.Request (List FileFormat)
fetchFormats =
    let
        url =
            apiBaseUrl ++ "/samples/files/formats"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list fileFormatDecoder))
        |> HttpBuilder.toRequest


fetchTypes : Http.Request (List FileType)
fetchTypes =
    let
        url =
            apiBaseUrl ++ "/samples/files/types"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list fileTypeDecoder))
        |> HttpBuilder.toRequest