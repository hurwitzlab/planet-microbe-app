module SamplingEvent exposing (SamplingEvent, Data, fetch, fetchAllByCampaign, fetchAllByProject, fetchAllBySample, fetchData)

{-| The interface to the Sample data structure.
-}

import Http
import HttpBuilder
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)
import LatLng exposing (LatLng)
import SearchTerm exposing (SearchTerm, PURL, Value)
import Config exposing (apiBaseUrl)



-- TYPES


type alias SamplingEvent  =
    { id : Int
    , type_ : String
    , name : String
    , locations : List LatLng
    , startTime : String
    , endTime : String
    , campaignId : Int
    , campaignType : String
    , campaignName : String
    , projectId : Int
    , projectName : String
    }


type alias Data =
    { schema_id : Int
    , terms : List SearchTerm
    , values : List (List (Maybe Value))
    }



-- SERIALIZATION


samplingEventDecoder : Decoder SamplingEvent
samplingEventDecoder =
    Decode.succeed SamplingEvent
        |> required "sampling_event_id" Decode.int
        |> required "sampling_event_type" Decode.string
        |> required "name" Decode.string
        |> optional "locations" (Decode.list LatLng.decoder) []
        |> optional "start_time" Decode.string ""
        |> optional "end_time" Decode.string ""
        |> optional "campaign_id" Decode.int 0
        |> optional "campaign_type" Decode.string ""
        |> optional "campaign_name" Decode.string ""
        |> optional "project_id" Decode.int 0
        |> optional "project_name" Decode.string ""


dataDecoder : Decoder Data
dataDecoder =
    Decode.succeed Data
        |> required "schema_id" Decode.int
        |> required "terms" (Decode.list SearchTerm.searchTermDecoder)
        |> required "values" (Decode.list (Decode.list SearchTerm.valueDecoder))



-- REQUESTS


fetch : Int -> Http.Request SamplingEvent
fetch id  =
    let
        url =
            apiBaseUrl ++ "/sampling_events/" ++ (String.fromInt id)
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson samplingEventDecoder)
        |> HttpBuilder.toRequest


fetchAllByCampaign : Int -> Http.Request (List SamplingEvent)
fetchAllByCampaign id =
    let
        url =
            apiBaseUrl ++ "/campaigns/" ++ (String.fromInt id) ++ "/sampling_events"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list samplingEventDecoder))
        |> HttpBuilder.toRequest


fetchAllByProject : Int -> Http.Request (List SamplingEvent)
fetchAllByProject id =
    let
        url =
            apiBaseUrl ++ "/projects/" ++ (String.fromInt id) ++ "/sampling_events"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list samplingEventDecoder))
        |> HttpBuilder.toRequest


fetchAllBySample : Int -> Http.Request (List SamplingEvent)
fetchAllBySample id =
    let
        url =
            apiBaseUrl ++ "/samples/" ++ (String.fromInt id) ++ "/sampling_events"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list samplingEventDecoder))
        |> HttpBuilder.toRequest


fetchData : Int -> String -> Http.Request Data
fetchData id type_ =
    let
        url =
            apiBaseUrl ++ "/sampling_events/" ++ (String.fromInt id) ++ "/data/" ++ type_
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson dataDecoder)
        |> HttpBuilder.toRequest