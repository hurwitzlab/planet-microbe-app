module Run exposing (Run, fetch, fetchAllByExperiment)

{-| The interface to the Run data structure.
-}

import Http
import HttpBuilder
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as Encode
import Config exposing (apiBaseUrl)
--import Debug exposing (toString)
import File exposing (File, fileDecoder)



-- TYPES


type alias Run  =
    { id : Int
    , accn : String
    , totalSpots : Int
    , totalBases : Int
    , experimentId : Int
    , experimentName : String
    , sampleId : Int
    , sampleAccn : String
    , files : List File
    }



-- SERIALIZATION


runDecoder : Decoder Run
runDecoder =
    Decode.succeed Run
        |> required "run_id" Decode.int
        |> required "accn" Decode.string
        |> required "total_spots" Decode.int
        |> required "total_bases" Decode.int
        |> optional "experiment_id" Decode.int 0
        |> optional "experiment_name" Decode.string ""
        |> optional "sample_id" Decode.int 0
        |> optional "sample_accn" Decode.string ""
        |> optional "files" (Decode.list fileDecoder) []



-- REQUESTS


fetch : Int -> Http.Request Run
fetch id  =
    let
        url =
            apiBaseUrl ++ "/runs/" ++ (String.fromInt id)
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson runDecoder)
        |> HttpBuilder.toRequest


fetchAllByExperiment : Int -> Http.Request (List Run)
fetchAllByExperiment id =
    let
        url =
            apiBaseUrl ++ "/experiments/" ++ (String.fromInt id) ++ "/runs"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list runDecoder))
        |> HttpBuilder.toRequest