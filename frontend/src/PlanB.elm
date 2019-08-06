module PlanB exposing (..)

{-| The interface to the PlanB job service
-}

import Agave
import Http
import HttpBuilder
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Config exposing (planbBaseUrl)



authorizationHeader : String -> ( String, String )
authorizationHeader token =
    ( "Authorization", "Bearer " ++ token)


getApp : String -> String -> Http.Request (Agave.Response Agave.App)
getApp token name =
    let
        url =
            planbBaseUrl ++ "/apps/" ++ name
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders [ authorizationHeader token ]
        |> HttpBuilder.withExpect (Http.expectJson (Agave.responseDecoder Agave.decoderApp))
        |> HttpBuilder.toRequest


getJobs : String -> Http.Request (Agave.Response (List Agave.Job))
getJobs token =
    let
        url =
            planbBaseUrl ++ "/jobs"
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders [ authorizationHeader token ]
        |> HttpBuilder.withExpect (Http.expectJson (Agave.responseDecoder (Decode.list Agave.decoderJob)))
        |> HttpBuilder.toRequest


getJob : String -> String -> Http.Request (Agave.Response Agave.Job)
getJob token id =
    let
        url =
            planbBaseUrl ++ "/jobs/" ++ id
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders [ authorizationHeader token ]
        |> HttpBuilder.withExpect (Http.expectJson (Agave.responseDecoder Agave.decoderJob))
        |> HttpBuilder.toRequest


getJobHistory : String -> String -> Http.Request (Agave.Response (List Agave.JobHistory))
getJobHistory token id =
    let
        url =
            planbBaseUrl ++ "/jobs/" ++ id ++ "/history"
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders [ authorizationHeader token ]
        |> HttpBuilder.withExpect (Http.expectJson (Agave.responseDecoder (Decode.list Agave.decoderJobHistory)))
        |> HttpBuilder.toRequest


launchJob : String -> Agave.JobRequest -> Http.Request (Agave.Response Agave.JobStatus)
launchJob token request =
    let
        url =
            planbBaseUrl ++ "/jobs"
    in
    HttpBuilder.post url
        |> HttpBuilder.withHeaders [ authorizationHeader token ]
        |> HttpBuilder.withJsonBody (Agave.encodeJobRequest request)
        |> HttpBuilder.withExpect (Http.expectJson (Agave.responseDecoder Agave.decoderJobStatus))
        |> HttpBuilder.toRequest


shareJob : String -> String -> String -> String -> Http.Request (Agave.Response Agave.JobStatus)
shareJob token id username permission =
    let
        url =
            planbBaseUrl ++ "/jobs/" ++ id ++ "/pems/" ++ username

        body =
            Encode.object
                [ ( "permission", Encode.string permission ) ]
    in
    HttpBuilder.post url
        |> HttpBuilder.withHeaders [ authorizationHeader token ]
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson (Agave.responseDecoder Agave.decoderJobStatus))
        |> HttpBuilder.toRequest
