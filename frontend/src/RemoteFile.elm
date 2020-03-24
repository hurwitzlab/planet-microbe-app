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
import Search
import Config exposing (apiBaseUrl)



-- TYPES --


type alias File =
    { id : Int
    , url : String
    , type_ : String
    , format : String
    , layout : String
    , source : String
    , strategy : String
    , selection : String
    , runAccn : String
    , experimentId : Int
    , experimentAccn : String
    , sampleId : Int
    , sampleAccn : String
    , projectId : Int
    , projectName : String
    }



-- SERIALIZATION --


fileDecoder : Decoder File
fileDecoder =
    Decode.succeed File
        |> required "file_id" Decode.int
        |> required "file_url" Decode.string
        |> required "file_type" Decode.string
        |> required "file_format" Decode.string
        |> required "layout" Decode.string
        |> required "source" Decode.string
        |> required "strategy" Decode.string
        |> required "selection" Decode.string
        |> required "run_accn" Decode.string
        |> required "experiment_id" Decode.int
        |> required "experiment_accn" Decode.string
        |> required "sample_id" Decode.int
        |> required "sample_accn" Decode.string
        |> required "project_id" Decode.int
        |> required "project_name" Decode.string



-- REQUESTS --


fetchAll : Http.Request (List File)
fetchAll =
    let
        url =
            apiBaseUrl ++ "/samples/files"
    in
    HttpBuilder.post url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list fileDecoder))
        |> HttpBuilder.toRequest


fetchSome : List Int -> Http.Request (List File)
fetchSome idList =
    let
        url =
            apiBaseUrl ++ "/samples/files"

        body =
            Encode.object
                [ ( "ids", Encode.string (idList |> List.map String.fromInt |> String.join ",") ) ]
    in
    HttpBuilder.post url
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list fileDecoder))
        |> HttpBuilder.toRequest


fetchProperties : Http.Request (List (String, Search.Distribution))
fetchProperties =
    let
        url =
            apiBaseUrl ++ "/samples/files/properties"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect
            (Http.expectJson (Decode.keyValuePairs (Search.distributionDecoder)))
        |> HttpBuilder.toRequest