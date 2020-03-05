module Sample exposing (Sample, Metadata, fetch, fetchAll, fetchSome, fetchAllByProject, fetchAllBySamplingEvent, fetchAllByCampaign, fetchMetadata, fetchSearchTerms, fetchSearchTerm)

{-| The interface to the Sample data structure.
-}

import Http
import HttpBuilder
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as Encode
import LatLng exposing (LatLng)
import SearchTerm exposing (SearchTerm, PURL, Value)
import Config exposing (apiBaseUrl)



-- TYPES --


type alias Sample  =
    { id : Int
    , accn : String
    , locations : List LatLng
    , projectId : Int
    , projectName : String
    }


type alias Metadata =
    { schema_id : Int
    , terms : List SearchTerm
    , values : List (Maybe Value)
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


metadataDecoder : Decoder Metadata
metadataDecoder =
    Decode.succeed Metadata
        |> required "schema_id" Decode.int
        |> required "terms" (Decode.list SearchTerm.searchTermDecoder)
        |> required "values" (Decode.list SearchTerm.valueDecoder)



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


fetchSearchTerms : Http.Request (List SearchTerm)
fetchSearchTerms =
    let
        url =
            apiBaseUrl ++ "/searchTerms" --FIXME change to samples/searchTerms
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list SearchTerm.searchTermDecoder))
        |> HttpBuilder.toRequest


fetchSearchTerm : PURL -> Http.Request SearchTerm
fetchSearchTerm id =
    let
        url =
            apiBaseUrl ++ "/searchTerms/" ++ id --FIXME change to samples/searchTerms
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson SearchTerm.searchTermDecoder)
        |> HttpBuilder.toRequest