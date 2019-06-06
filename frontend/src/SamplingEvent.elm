module SamplingEvent exposing (SamplingEvent, fetch, fetchAllByCampaign, fetchAllByProject)

{-| The interface to the Sample data structure.
-}

import Http
import HttpBuilder
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as Encode
import LatLng exposing (LatLng)
import Config exposing (apiBaseUrl)
import Debug exposing (toString)



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



-- SERIALIZATION


samplingEventDecoder : Decoder SamplingEvent
samplingEventDecoder =
    Decode.succeed SamplingEvent
        |> required "sampling_event_id" Decode.int
        |> required "sampling_event_type" Decode.string
        |> required "name" Decode.string
        |> required "locations" (Decode.list LatLng.decoder)
        |> optional "start_time" Decode.string ""
        |> optional "end_time" Decode.string ""
        |> optional "campaign_id" Decode.int 0
        |> optional "campaign_type" Decode.string ""
        |> optional "campaign_name" Decode.string ""
        |> optional "project_id" Decode.int 0
        |> optional "project_name" Decode.string ""



-- REQUESTS


fetch : Int -> Http.Request SamplingEvent
fetch id  =
    let
        url =
            apiBaseUrl ++ "/sampling_events/" ++ (toString id)
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson samplingEventDecoder)
        |> HttpBuilder.toRequest


fetchAllByCampaign : Int -> Http.Request (List SamplingEvent)
fetchAllByCampaign id =
    let
        url =
            apiBaseUrl ++ "/campaigns/" ++ (toString id) ++ "/sampling_events"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list samplingEventDecoder))
        |> HttpBuilder.toRequest


fetchAllByProject : Int -> Http.Request (List SamplingEvent)
fetchAllByProject id =
    let
        url =
            apiBaseUrl ++ "/projects/" ++ (toString id) ++ "/sampling_events"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list samplingEventDecoder))
        |> HttpBuilder.toRequest