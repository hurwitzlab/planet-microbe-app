module Sample exposing (Sample, PURL, Metadata, Value(..), SearchTerm, Annotation, annotationsToHide, fetch, fetchAll, fetchAllByProject, fetchAllBySamplingEvent, fetchAllByCampaign, fetchMetadata, fetchSearchTerms, fetchSearchTerm)

{-| The interface to the Sample data structure.
-}

import Http
import HttpBuilder
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as Encode
import Dict exposing (Dict)
import LatLng exposing (LatLng)
import Config exposing (apiBaseUrl)



-- TYPES


type alias Sample  =
    { id : Int
    , accn : String
    , locations : List LatLng
    , projectId : Int
    , projectName : String
    }


type alias PURL =
    String


type alias Metadata =
    { schema_id : Int
    , terms : List SearchTerm
    , values : List (Maybe Value)
    }


type Value
    = StringValue String
    | IntValue Int
    | FloatValue Float


type alias SearchTerm =
    { type_ : String
    , id : PURL
    , label : String
    , definition : String
    , unitId : PURL
    , unitLabel : String
    , sourceUrl : String
    , alias_ : String
    , aliases : List String
    , min : Float
    , max : Float
    , values : Dict String Int --FIXME change to List (String, Int)
    , annotations : List Annotation
    }


type alias Annotation =
    { id : String
    , label : String
    , value : String
    }


annotationsToHide =
    [ "http://purl.obolibrary.org/obo/IAO_0000116" -- editor's note
    ]



-- SERIALIZATION


sampleDecoder : Decoder Sample
sampleDecoder =
    Decode.succeed Sample
        |> required "sample_id" Decode.int
        |> required "accn" Decode.string
        |> required "locations" (Decode.list LatLng.decoder)
        |> optional "project_id" Decode.int 0
        |> optional "project_name" Decode.string ""


metadataDecoder : Decoder Metadata
metadataDecoder =
    Decode.succeed Metadata
        |> required "schema_id" Decode.int
        |> required "terms" (Decode.list searchTermDecoder)
        |> required "values" (Decode.list valueDecoder)


valueDecoder : Decoder (Maybe Value)
valueDecoder =
    Decode.nullable
        (Decode.oneOf
            [ Decode.map StringValue Decode.string
            , Decode.map IntValue Decode.int
            , Decode.map FloatValue Decode.float
            ]
        )


searchTermDecoder : Decoder SearchTerm
searchTermDecoder =
    Decode.succeed SearchTerm
        |> required "type" Decode.string
        |> required "id" Decode.string
        |> required "label" Decode.string
        |> optional "definition" Decode.string ""
        |> optional "unitId" Decode.string ""
        |> optional "unitLabel" Decode.string ""
        |> optional "sourceUrl" Decode.string ""
        |> optional "alias" Decode.string ""
        |> optional "aliases" (Decode.list Decode.string) []
        |> optional "min" Decode.float 0
        |> optional "max" Decode.float 0
        |> optional "values" (Decode.dict Decode.int) Dict.empty
        |> optional "annotations" (Decode.list annotationDecoder) []


annotationDecoder : Decoder Annotation
annotationDecoder =
    Decode.succeed Annotation
        |> required "id" Decode.string
        |> required "label" Decode.string
        |> required "value" Decode.string



-- REQUESTS


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
    HttpBuilder.get url
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
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list searchTermDecoder))
        |> HttpBuilder.toRequest


fetchSearchTerm : PURL -> Http.Request SearchTerm
fetchSearchTerm id =
    let
        url =
            apiBaseUrl ++ "/searchTerms/" ++ id --FIXME change to samples/searchTerms
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson searchTermDecoder)
        |> HttpBuilder.toRequest