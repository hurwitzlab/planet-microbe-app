module RemoteFile exposing (..)

{-| The interface to the File data structure.

Represents a data file in the Data Store with a type and a format.

Had to rename this from File to RemoteFile due to conflict with Elm's File package (elm/file).
-}

import Http
import HttpBuilder
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as Encode
import Dict exposing (Dict)
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


fetchProperties : Http.Request (Dict String (List (String, Int)))
fetchProperties =
    let
        url =
            apiBaseUrl ++ "/samples/files/properties"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect
            (Http.expectJson
                (Decode.dict
                    (Decode.list (Decode.map2 Tuple.pair (Decode.index 0 Decode.string) (Decode.index 1 Decode.int)))))
        |> HttpBuilder.toRequest