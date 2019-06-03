module Campaign exposing (Campaign, fetch)

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


type alias Campaign  =
    { id : Int
    , type_ : String
    , name : String
    , description : String
    , deployment : String
    , startLocation : String
    , endLocation : String
    , startTime : String
    , endTime : String
    , urls : List String
    }



-- SERIALIZATION


campaignDecoder : Decoder Campaign
campaignDecoder =
    Decode.succeed Campaign
        |> required "campaign_id" Decode.int
        |> required "campaign_type" Decode.string
        |> required "name" Decode.string
        |> optional "description" Decode.string ""
        |> optional "deployment" Decode.string ""
        |> optional "start_location" Decode.string ""
        |> optional "end_location" Decode.string ""
        |> optional "start_time" Decode.string ""
        |> optional "end_time" Decode.string ""
        |> optional "urls" (Decode.list Decode.string) []



-- REQUESTS


fetch : Int -> Http.Request Campaign
fetch id  =
    let
        url =
            apiBaseUrl ++ "/campaigns/" ++ (toString id)
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson campaignDecoder)
        |> HttpBuilder.toRequest
