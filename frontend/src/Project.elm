module Project exposing (Project, fetch, fetchAll, fetchCounts)

{-| The interface to the Project data structure.
-}

import Http
import HttpBuilder
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)
import Config exposing (apiBaseUrl)
import RemoteFile exposing (File, fileDecoder)



-- TYPES


type alias Project  =
    { id : Int
    , name : String
    , accn : String
    , description : String
    , type_ : String
    , sampleCount : Int
    , datapackageUrl : String
    , url : String
    , files : List File
    }



-- SERIALIZATION


projectDecoder : Decoder Project
projectDecoder =
    Decode.succeed Project
        |> required "project_id" Decode.int
        |> required "name" Decode.string
        |> required "accn" Decode.string
        |> required "description" Decode.string
        |> required "type" Decode.string
        |> required "sample_count" Decode.int
        |> optional "datapackage_url" Decode.string ""
        |> optional "project_url" Decode.string ""
        |> optional "files" (Decode.list fileDecoder) []



-- REQUESTS


fetch : Int -> Http.Request Project
fetch id  =
    let
        url =
            apiBaseUrl ++ "/projects/" ++ (String.fromInt id)
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson projectDecoder)
        |> HttpBuilder.toRequest


fetchAll : Http.Request (List Project)
fetchAll =
    let
        url =
            apiBaseUrl ++ "/projects"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list projectDecoder))
        |> HttpBuilder.toRequest


fetchCounts : Http.Request (List (String, Int))
fetchCounts =
    let
        url =
            apiBaseUrl ++ "/projects"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect
            (Http.expectJson
                (Decode.list (Decode.map2 Tuple.pair (Decode.field "name" Decode.string) (Decode.field "sample_count" Decode.int))))
        |> HttpBuilder.toRequest