module Sample exposing (Sample, fetch, fetchAll, fetchAllByProject)

{-| The interface to the Sample data structure.
-}

import Http
import HttpBuilder
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as Encode
import Config exposing (apiBaseUrl)
import Debug exposing (toString)



-- TYPES


type alias Sample  =
    { id : Int
    , accn : String
    , samplingEventId : Int
    , samplingEventType : String
    , samplingEventName : String
    , campaignId : Int
    , campaignName : String
    , campaignType : String
    , projectId : Int
    , projectName : String
    }



-- SERIALIZATION


sampleDecoder : Decoder Sample
sampleDecoder =
    Decode.succeed Sample
        |> required "sample_id" Decode.int
        |> required "accn" Decode.string
        |> optional "sampling_event_id" Decode.int 0
        |> optional "sampling_event_type" Decode.string ""
        |> optional "sampling_event_name" Decode.string ""
        |> optional "campaign_id" Decode.int 0
        |> optional "campaign_name" Decode.string ""
        |> optional "campaign_type" Decode.string ""
        |> optional "project_id" Decode.int 0
        |> optional "project_name" Decode.string ""



-- REQUESTS


fetch : Int -> Http.Request Sample
fetch id  =
    let
        url =
            apiBaseUrl ++ "/samples/" ++ (toString id)
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
            apiBaseUrl ++ "/projects/" ++ (toString id) ++ "/samples"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list sampleDecoder))
        |> HttpBuilder.toRequest