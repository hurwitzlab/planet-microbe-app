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
    , name : String
    }



-- SERIALIZATION


campaignDecoder : Decoder Campaign
campaignDecoder =
    Decode.succeed Campaign
        |> required "campaign_id" Decode.int
        |> required "name" Decode.string



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
