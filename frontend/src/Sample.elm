module Sample exposing (..)

{-| The interface to the Sample data structure.
-}

import Http
import HttpBuilder
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as Encode
import LatLng exposing (LatLng)
import Search exposing (SearchTerm, Value)
import Config exposing (apiBaseUrl)



-- TYPES --


type alias Sample  =
    { id : Int
    , accn : String
    , locations : List LatLng
    , projectId : Int
    , projectName : String
    , files : List Int
    }


type alias Metadata =
    { schema_id : Int
    , terms : List SearchTerm
    , values : List (Maybe Value)
    }


type alias Taxonomy =
    { experimentId : Int
    , experimentAccn : String
    , runId : Int
    , runAccn : String
    , taxId : Int
    , speciesName : String
    , numReads : Int
    , numUniqueReads : Int
    , abundance : Float
    }



-- SERIALIZATION --


sampleDecoder : Decoder Sample
sampleDecoder =
    Decode.succeed Sample
        |> required "sample_id" Decode.int
        |> required "accn" Decode.string
        |> optional "locations" (Decode.list LatLng.decoder) []
        |> optional "project_id" Decode.int 0
        |> optional "project_name" Decode.string ""
        |> optional "files" (Decode.list Decode.int) []


metadataDecoder : Decoder Metadata
metadataDecoder =
    Decode.succeed Metadata
        |> required "schema_id" Decode.int
        |> required "terms" (Decode.list Search.searchTermDecoder)
        |> required "values" (Decode.list Search.valueDecoder)


taxonomyDecoder : Decoder Taxonomy
taxonomyDecoder =
    Decode.succeed Taxonomy
        |> required "experiment_id" Decode.int
        |> required "experiment_accn" Decode.string
        |> required "run_id" Decode.int
        |> required "run_accn" Decode.string
        |> required "tax_id" Decode.int
        |> required "name" Decode.string
        |> required "num_reads" Decode.int
        |> required "num_unique_reads" Decode.int
        |> required "abundance" Decode.float



-- REQUESTS --


fetch : Int -> Http.Request Sample
fetch id  =
    let
        url =
            apiBaseUrl ++ "/samples/" ++ (String.fromInt id)
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson sampleDecoder)
        |> HttpBuilder.toRequest


fetchAll : Http.Request (List Sample)
fetchAll =
    let
        url =
            apiBaseUrl ++ "/samples"
    in
    HttpBuilder.post url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list sampleDecoder))
        |> HttpBuilder.toRequest


fetchSome : List Int -> Http.Request (List Sample)
fetchSome idList =
    let
        url =
            apiBaseUrl ++ "/samples"

        body =
            Encode.object
                [ ( "ids", Encode.string (idList |> List.map String.fromInt |> String.join ",") ) ]
    in
    HttpBuilder.post url
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list sampleDecoder))
        |> HttpBuilder.toRequest


fetchAllByProject : Int -> Http.Request (List Sample)
fetchAllByProject id =
    let
        url =
            apiBaseUrl ++ "/projects/" ++ (String.fromInt id) ++ "/samples"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list sampleDecoder))
        |> HttpBuilder.toRequest


fetchAllBySamplingEvent : Int -> Http.Request (List Sample)
fetchAllBySamplingEvent id =
    let
        url =
            apiBaseUrl ++ "/sampling_events/" ++ (String.fromInt id) ++ "/samples"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list sampleDecoder))
        |> HttpBuilder.toRequest


fetchAllByCampaign : Int -> Http.Request (List Sample)
fetchAllByCampaign id =
    let
        url =
            apiBaseUrl ++ "/campaigns/" ++ (String.fromInt id) ++ "/samples"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list sampleDecoder))
        |> HttpBuilder.toRequest


fetchMetadata : Int -> Http.Request Metadata
fetchMetadata id =
    let
        url =
            apiBaseUrl ++ "/samples/" ++ (String.fromInt id) ++ "/metadata"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson metadataDecoder)
        |> HttpBuilder.toRequest


fetchTaxonomy : Int -> Http.Request (List Taxonomy)
fetchTaxonomy id =
    let
        url =
            apiBaseUrl ++ "/samples/" ++ (String.fromInt id) ++ "/taxonomy"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list taxonomyDecoder))
        |> HttpBuilder.toRequest