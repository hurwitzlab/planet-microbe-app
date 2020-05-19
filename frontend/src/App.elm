module App exposing (..)

{-| The interface to the App data structure.
-}

import Http
import HttpBuilder
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as Encode
import Config exposing (apiBaseUrl)



-- TYPES --


type alias App =
    { id : Int
    , name : String
    , is_active : Bool
    , is_maintenance : Bool
    , provider : String
--    , app_tags : List AppTag
--    , app_data_types : List AppDataType
    , app_results : List AppResult
    }


type alias AppRun =
    { app_run_id : Int
    , app_id : Int
    , user_id : Maybe Int
    , app_ran_at : String
    , params : String
    }


type alias AppTag =
    { app_tag_id : Int
    , value : String
    }


type alias AppDataType =
    { app_data_type_id : Int
    , name : String
    , alias_ : String
    }


type alias AppResult =
    { app_result_id : Int
    , path : String
    , app_data_type : AppDataType
    }


type alias FileBrowser =
    { id : String
    , username : String
    , token : String
    , path : String
    , source : String
    }



-- SERIALIZATION --


appDecoder : Decoder App
appDecoder =
    Decode.succeed App
        |> required "app_id" Decode.int
        |> required "name" Decode.string
        |> required "is_active" Decode.bool
        |> required "is_maintenance" Decode.bool
        |> optional "provider" Decode.string ""
--        |> optional "app_tags" (Decode.list appTagDecoder) []
--        |> optional "app_data_types" (Decode.list appDataTypeDecoder) []
        |> optional "app_results" (Decode.list appResultDecoder) []


appRunDecoder : Decoder AppRun
appRunDecoder =
    Decode.succeed AppRun
        |> required "app_run_id" Decode.int
        |> required "app_id" Decode.int
        |> optional "user_id" (Decode.nullable Decode.int) Nothing
        |> optional "app_ran_at" Decode.string ""
        |> optional "params" Decode.string ""


appTagDecoder : Decoder AppTag
appTagDecoder =
    Decode.succeed AppTag
        |> required "app_tag_id" Decode.int
        |> required "value" Decode.string


appDataTypeDecoder : Decoder AppDataType
appDataTypeDecoder =
    Decode.succeed AppDataType
        |> required "app_data_type_id" Decode.int
        |> required "name" Decode.string
        |> optional "alias" Decode.string ""


appResultDecoder : Decoder AppResult
appResultDecoder =
    Decode.succeed AppResult
        |> required "app_result_id" Decode.int
        |> required "path" Decode.string
        |> required "app_data_type" appDataTypeDecoder


encodeAppRun : Int -> String -> Encode.Value
encodeAppRun app_id params =
    Encode.object
        [ ( "app_id", Encode.int app_id )
        , ( "params", Encode.string params )
        ]



-- REQUESTS --


fetch : Int -> Http.Request App
fetch id  =
    let
        url =
            apiBaseUrl ++ "/apps/" ++ (String.fromInt id)
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson appDecoder)
        |> HttpBuilder.toRequest


fetchAll : Http.Request (List App)
fetchAll =
    let
        url =
            apiBaseUrl ++ "/apps"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list appDecoder))
        |> HttpBuilder.toRequest


fetchByName : String -> Http.Request App
fetchByName name =
    HttpBuilder.get (apiBaseUrl ++ "/apps/" ++ name)
        |> HttpBuilder.withExpect (Http.expectJson appDecoder)
        |> HttpBuilder.toRequest


run : String -> Int -> String -> Http.Request AppRun
run token app_id params =
    let
        url =
            apiBaseUrl ++ "/apps/runs"

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.post url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withJsonBody (encodeAppRun app_id params)
        |> HttpBuilder.withExpect (Http.expectJson appRunDecoder)
        |> HttpBuilder.toRequest