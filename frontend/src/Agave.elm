module Agave exposing (..) -- TODO make elm package of this module and import that way

{-| The interface to the Tapis/Agave
-}

import Http
import HttpBuilder
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)
import Dict exposing (Dict)
import Config exposing (agaveBaseUrl)


-- TYPES --


type alias Response a =
    { status : String
    , result : a
    }


type alias EmptyResponse =
    { status : String
    }


type alias TokenResponse =
    { accessToken : String
    , expiresIn : Int
    , refreshToken : String
    , tokenType: String
    }


type alias Profile =
    { email : String
    , first_name : String
    , last_name : String
    , username : String
    , uid : Int
    }


type alias App =
    { name : String
    , helpURI : String
    , shortDescription : String
    , version : String
    , tags : List String
    , isPublic : Bool
    , inputs : List AppInput
    , parameters : List AppParameter
    }


type alias AppInput =
    { id : String
    , details : Details
    , value : InputValue
    , semantics : Semantics
    }


type alias AppParameter =
    { id : String
    , details : Details
    , value : ParameterValue
    }


type alias Details =
    { label : String
    , argument : String
    , description : String
    }


type alias Semantics =
    { filesTypes : List String
    , minCardinality : Int
    , maxCardinality : Int
    , ontology : List String
    }


type alias InputValue =
    { order : Int
    , default : ValueType
    , required : Bool
    , visible : Bool
    }


type alias ParameterValue = -- TODO change this to use variable decoding per http://folkertdev.nl/blog/elm-messy-json-value/, see ValueType in this file as example
    { order : Int
    , visible : Bool
    , type_ : String
    , default : ValueType
    , enum_values : Maybe (List (List (String, String)))
    }


type ValueType
    = StringValue String
    | ArrayValue (List String)
    | BoolValue Bool -- workaround for BowtieBatch app
    | NumberValue Float


type alias JobRequest =
    { name : String
    , app_id : String
    , archive : Bool
    , inputs : List JobInput
    , parameters : List JobParameter
    , notifications : List Notification
    }


type alias JobInput =
    { id : String
    , value : List String
    }


type alias JobParameter =
    { id : String
    , value : ValueType
    }


type alias Notification =
    { url : String
    , event : String
    }


type alias Job =
    { id : String
    , name : String
    , owner : String
    , app_id : String
    , startTime : String
    , endTime : String
    , status : String
    , inputs : Dict String JobInputValue
    , parameters : Dict String ValueType
    }


type JobInputValue
    = JobInputString String
    | JobInputArray (List String)


type alias JobStatus =
    { id : String
    }


type alias JobError =
    { status : String
    , message : String
    }


type alias JobOutput =
    { name : String
    , path : String
    , type_ : String
    }


type alias JobHistory =
    { status : String
    , created : String
    , createdBy : String
    , description : String
    }


type alias FileResult =
    { name : String
    , path : String
    , type_ : String
    , format : String
    , mimeType : String
    , lastModified : String
    , length : Int
    }


type alias UploadResult =
    { name : String
    , path : String
    }


type alias PermissionResult =
    { username : String
    , permission : Permission
    , recursive : Bool
    }


type alias Permission =
    { read : Bool
    , write : Bool
    , execute : Bool
    }



-- SERIALIZATION --


responseDecoder : Decoder a -> Decoder (Response a)
responseDecoder decoder =
    Decode.succeed Response
        |> required "status" Decode.string --TODO make sure status is "success"
        |> required "result" decoder


emptyResponseDecoder : Decoder EmptyResponse
emptyResponseDecoder =
    Decode.succeed EmptyResponse
        |> required "status" Decode.string --TODO make sure status is "success"


tokenResponseDecoder : Decoder TokenResponse
tokenResponseDecoder =
    Decode.succeed TokenResponse
        |> required "access_token" Decode.string
        |> required "expires_in" Decode.int
        |> required "refresh_token" Decode.string
        |> required "token_type" Decode.string


decoderProfile : Decoder Profile
decoderProfile =
    Decode.succeed Profile
        |> required "email" Decode.string
        |> required "first_name" Decode.string
        |> required "last_name" Decode.string
        |> required "username" Decode.string
        |> required "uid" Decode.int


decoderApp : Decoder App
decoderApp =
    Decode.succeed App
        |> required "name" Decode.string
        |> required "helpURI" Decode.string
        |> required "shortDescription" Decode.string
        |> required "version" Decode.string
        |> optional "tags" (Decode.list Decode.string) []
        |> optional "isPublic" Decode.bool False
        |> required "inputs" (Decode.list decoderAppInput)
        |> required "parameters" (Decode.list decoderAppParameter)


decoderAppInput : Decoder AppInput
decoderAppInput =
    Decode.succeed AppInput
        |> required "id" Decode.string
        |> required "details" decoderDetails
        |> required "value" decoderInputValue
        |> required "semantics" decoderSemantics


decoderAppParameter : Decoder AppParameter
decoderAppParameter =
    Decode.succeed AppParameter
        |> required "id" Decode.string
        |> required "details" decoderDetails
        |> required "value" decoderParameterValue


decoderDetails : Decoder Details
decoderDetails =
    Decode.succeed Details
        |> required "label" Decode.string
        |> optional "argument" Decode.string ""
        |> optional "description" Decode.string ""


decoderSemantics : Decoder Semantics
decoderSemantics =
    Decode.succeed Semantics
        |> required "fileTypes" (Decode.list Decode.string)
        |> optional "minCardinality" Decode.int 0
        |> optional "maxCardinality" Decode.int 0
        |> optional "ontology" (Decode.list Decode.string) []


decoderInputValue : Decoder InputValue
decoderInputValue =
    Decode.succeed InputValue
        |> required "order" Decode.int
        |> optional "default" decoderValueType (StringValue "")
        |> optional "required" Decode.bool True
        |> optional "visible" Decode.bool True


decoderParameterValue : Decoder ParameterValue
decoderParameterValue =
    Decode.succeed ParameterValue
        |> required "order" Decode.int
        |> optional "visible" Decode.bool True
        |> required "type" Decode.string
        |> optional "default" decoderValueType (StringValue "")
        |> optional "enum_values" (Decode.nullable (Decode.list (Decode.keyValuePairs Decode.string))) Nothing


decoderValueType : Decoder ValueType
decoderValueType =
    Decode.oneOf
        [ Decode.map StringValue Decode.string
        , Decode.map ArrayValue (Decode.list Decode.string)
        , Decode.map BoolValue Decode.bool
        , Decode.map NumberValue Decode.float
        ]


decoderJobStatus : Decoder JobStatus
decoderJobStatus =
    Decode.succeed JobStatus
        |> required "id" Decode.string


decoderJobError : Decoder JobError
decoderJobError =
    Decode.succeed JobError
        |> required "status" Decode.string
        |> required "message" Decode.string


decoderJob : Decoder Job
decoderJob =
    Decode.succeed Job
        |> required "id" Decode.string
        |> required "name" Decode.string
        |> optional "owner" Decode.string ""
        |> required "appId" Decode.string
        |> optional "startTime" Decode.string ""
        |> optional "endTime" Decode.string ""
        |> optional "status" Decode.string ""
        |> optional "inputs" (Decode.dict decoderJobInput) Dict.empty
        |> optional "parameters" (Decode.dict decoderValueType) Dict.empty


decoderJobInput : Decoder JobInputValue
decoderJobInput =
    Decode.oneOf
        [ Decode.map JobInputString Decode.string
        , Decode.map JobInputArray (Decode.list Decode.string)
        ]


decoderJobOutput : Decoder JobOutput
decoderJobOutput =
    Decode.succeed JobOutput
        |> required "name" Decode.string
        |> required "path" Decode.string
        |> required "type" Decode.string


decoderJobHistory : Decoder JobHistory
decoderJobHistory =
    Decode.succeed JobHistory
        |> required "status" Decode.string
        |> required "created" Decode.string
        |> required "createdBy" Decode.string
        |> required "description" Decode.string


decoderFileResult : Decoder FileResult
decoderFileResult =
    Decode.succeed FileResult
        |> required "name" Decode.string
        |> required "path" Decode.string
        |> required "type" Decode.string
        |> required "format" Decode.string
        |> required "mimeType" Decode.string
        |> required "lastModified" Decode.string
        |> required "length" Decode.int


decoderUploadResult : Decoder UploadResult
decoderUploadResult =
    Decode.succeed UploadResult
        |> required "name" Decode.string
        |> required "path" Decode.string


decoderPermissionResult : Decoder PermissionResult
decoderPermissionResult =
    Decode.succeed PermissionResult
        |> required "username" Decode.string
        |> required "permission" decoderPermission
        |> required "recursive" Decode.bool


decoderPermission : Decoder Permission
decoderPermission =
    Decode.succeed Permission
        |> required "read" Decode.bool
        |> required "write" Decode.bool
        |> required "execute" Decode.bool


encodeProfile : Profile -> Value
encodeProfile profile =
    Encode.object
        [ ( "email", Encode.string profile.email )
        , ( "first_name", Encode.string profile.first_name )
        , ( "last_name", Encode.string profile.last_name )
        , ( "username", Encode.string profile.username )
        ]


encodeJobRequest : JobRequest -> Encode.Value
encodeJobRequest request =
    Encode.object
        [ ( "name", Encode.string request.name )
        , ( "appId", Encode.string request.app_id )
        , ( "archive", Encode.bool request.archive )
--        , ( "inputs", Encode.list encodeJobInput request.inputs )
        , ( "inputs", Encode.object (List.map (\i -> (i.id, (Encode.list Encode.string i.value))) request.inputs) )
        , ( "parameters", Encode.object (List.map encodeJobParameter request.parameters) )
--        , ( "parameters", Encode.object (List.map (\p-> (p.id, (Encode.string p.value))) request.parameters) )
        , ( "notifications", Encode.list encodeNotification request.notifications )
        ]


encodeJobInput : JobInput -> Encode.Value
encodeJobInput input =
    Encode.object
        [ ( input.id, Encode.list  Encode.string input.value ) ]


encodeJobParameter : JobParameter -> (String, Encode.Value)
encodeJobParameter param =
    ( param.id, encodeValueType param.value )


encodeValueType : ValueType -> Encode.Value
encodeValueType value =
    case value of
        StringValue s ->
            Encode.string s

        ArrayValue l ->
            Encode.list Encode.string l

        BoolValue b ->
            Encode.bool b

        NumberValue n ->
            Encode.float n


encodeNotification : Notification -> Encode.Value
encodeNotification notification =
    Encode.object
        [ ( "url", Encode.string notification.url )
        , ( "event", Encode.string notification.event )
        ]



-- REQUESTS --


authorizationHeader : String -> ( String, String )
authorizationHeader token =
    ( "Authorization", "Bearer " ++ token)


getProfile : String -> Http.Request (Response Profile)
getProfile token =
    let
        url =
            agaveBaseUrl ++ "/profiles/v2/me"
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders [ authorizationHeader token ]
        |> HttpBuilder.withExpect (Http.expectJson (responseDecoder decoderProfile))
        |> HttpBuilder.toRequest


searchProfiles : String -> String -> Http.Request (Response (List Profile))
searchProfiles token username =
    let
        url =
            agaveBaseUrl ++ "/profiles/v2"

        queryParams =
            [( "username", username )]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders [ authorizationHeader token ]
        |> HttpBuilder.withQueryParams queryParams
        |> HttpBuilder.withExpect (Http.expectJson (responseDecoder (Decode.list decoderProfile)))
        |> HttpBuilder.toRequest


getApp : String -> String -> Http.Request (Response App)
getApp token name =
    let
        url =
            agaveBaseUrl ++ "/apps/v2/" ++ name
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders [ authorizationHeader token ]
        |> HttpBuilder.withExpect (Http.expectJson (responseDecoder decoderApp))
        |> HttpBuilder.toRequest


getJobs : String -> Http.Request (Response (List Job))
getJobs token =
    let
        url =
            agaveBaseUrl ++ "/jobs/v2/"
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders [ authorizationHeader token ]
        |> HttpBuilder.withExpect (Http.expectJson (responseDecoder (Decode.list decoderJob)))
        |> HttpBuilder.toRequest


getJob : String -> String -> Http.Request (Response Job)
getJob token id =
    let
        url =
            agaveBaseUrl ++ "/jobs/v2/" ++ id
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders [ authorizationHeader token ]
        |> HttpBuilder.withExpect (Http.expectJson (responseDecoder decoderJob))
        |> HttpBuilder.toRequest


getJobHistory : String -> String -> Http.Request (Response (List JobHistory))
getJobHistory token id =
    let
        url =
            agaveBaseUrl ++ "/jobs/v2/" ++ id ++ "/history"
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders [ authorizationHeader token ]
        |> HttpBuilder.withExpect (Http.expectJson (responseDecoder (Decode.list decoderJobHistory)))
        |> HttpBuilder.toRequest


getJobOutputs : String -> String -> String -> Maybe String -> Http.Request (Response (List JobOutput))
getJobOutputs username token id path =
    let
        baseUrl =
            -- Changed Agave endpoint for PlanB support
            --agaveBaseUrl ++ "/jobs/v2/" ++ id ++ "/outputs/listings"
            agaveBaseUrl ++ "/files/v2/listings/" ++ username ++ "/archive/jobs/job-" ++ id

        url =
            case path of
                Nothing ->
                    baseUrl

                Just path2 ->
                    baseUrl ++ "/" ++ path2

        queryParams =
            [("limit", "9999")]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders [ authorizationHeader token ]
        |> HttpBuilder.withQueryParams queryParams
        |> HttpBuilder.withExpect (Http.expectJson (responseDecoder (Decode.list decoderJobOutput)))
        |> HttpBuilder.toRequest


getFileList : String -> String -> Http.Request (Response (List FileResult))
getFileList token path =
    let
        url =
            agaveBaseUrl ++ "/files/v2/listings/" ++ path

        queryParams =
            [("limit", "9999")]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders [ authorizationHeader token ]
        |> HttpBuilder.withQueryParams queryParams
        |> HttpBuilder.withExpect (Http.expectJson (responseDecoder (Decode.list decoderFileResult)))
        |> HttpBuilder.toRequest


getFileRange : String -> String -> Maybe (Int, Int) -> Http.Request String
getFileRange token path range =
    let
        url =
            -- Changed Agave endpoint after adding archive=True
            --agaveBaseUrl ++ "/jobs/v2/" ++ id ++ "/outputs/media/" ++ path
            agaveBaseUrl ++ "/files/v2/media/" ++ (removeTrailingSlash path)

        authHeader =
            ( "Authorization", token)

        headers =
            case range of
                Nothing ->
                    [ authorizationHeader token ]
                Just (start, end) ->
                    [ authorizationHeader token
                    , ( "Range", "bytes=" ++ (String.fromInt start) ++ "-" ++ (String.fromInt end) )
                    ]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect Http.expectString
        |> HttpBuilder.toRequest


getFile : String -> String -> Http.Request String
getFile token path =
    getFileRange token path Nothing


getJobOutput : String -> String -> String -> String -> Http.Request String
getJobOutput username token id path =
    let
        jobPath =
            username ++ "/archive/jobs/job-" ++ id ++ "/" ++ path
    in
    getFile token jobPath


launchJob : String -> JobRequest -> Http.Request (Response JobStatus)
launchJob token request =
    let
        url =
            agaveBaseUrl ++ "/jobs/v2"
    in
    HttpBuilder.post url
        |> HttpBuilder.withHeaders [ authorizationHeader token ]
        |> HttpBuilder.withJsonBody (encodeJobRequest request)
        |> HttpBuilder.withExpect (Http.expectJson (responseDecoder decoderJobStatus))
        |> HttpBuilder.toRequest


shareJob : String -> String -> String -> String -> Http.Request (Response JobStatus)
shareJob token id username permission =
    let
        url =
            agaveBaseUrl ++ "/jobs/v2/" ++ id ++ "/pems/" ++ username

        body =
            Encode.object
                [ ( "permission", Encode.string permission ) ]
    in
    HttpBuilder.post url
        |> HttpBuilder.withHeaders [ authorizationHeader token ]
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson (responseDecoder decoderJobStatus))
        |> HttpBuilder.toRequest


stopJob : String -> String -> Http.Request String
stopJob token id =
    let
        url =
            agaveBaseUrl ++ "/jobs/v2/" ++ id

        headers =
            [ authorizationHeader token
            , ("Content-type", "application/x-www-form-urlencoded")
            ]

        body =
            "action=stop"
    in
    HttpBuilder.post url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withStringBody "" body
        |> HttpBuilder.withExpect Http.expectString
        |> HttpBuilder.toRequest


mkdir : String -> String -> String -> Http.Request EmptyResponse
mkdir token path dirname =
    let
        url =
            agaveBaseUrl ++ "/files/v2/media/" ++ (removeTrailingSlash path)

        body =
            Encode.object
                [ ( "action", Encode.string "mkdir" )
                , ( "path", Encode.string dirname )
                ]
    in
    HttpBuilder.put url
        |> HttpBuilder.withHeaders [ authorizationHeader token ]
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson emptyResponseDecoder)
        |> HttpBuilder.toRequest


delete : String -> String -> Http.Request EmptyResponse
delete token path =
    let
        url =
            agaveBaseUrl ++ "/files/v2/media/" ++ (removeTrailingSlash path)
    in
    HttpBuilder.delete url
        |> HttpBuilder.withHeaders [ authorizationHeader token ]
        |> HttpBuilder.withExpect (Http.expectJson emptyResponseDecoder)
        |> HttpBuilder.toRequest


getFilePermission : String -> String -> Http.Request (Response (List PermissionResult))
getFilePermission token path =
    let
        url =
            agaveBaseUrl ++ "/files/v2/pems/system/data.iplantcollaborative.org/" ++ (removeTrailingSlash path)
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders [ authorizationHeader token ]
        |> HttpBuilder.withExpect (Http.expectJson (responseDecoder (Decode.list decoderPermissionResult)))
        |> HttpBuilder.toRequest


setFilePermission : String -> String -> String -> String -> Http.Request EmptyResponse
setFilePermission token username permission path =
    let
        url =
            agaveBaseUrl ++ "/files/v2/pems/system/data.iplantcollaborative.org/" ++ (removeTrailingSlash path)

        body =
            Encode.object
                [ ( "username", Encode.string username )
                , ( "permission", Encode.string permission )
                , ( "recursive", Encode.bool True )
                ]
    in
    HttpBuilder.post url
        |> HttpBuilder.withHeaders [ authorizationHeader token ]
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson emptyResponseDecoder)
        |> HttpBuilder.toRequest


removeTrailingSlash : String -> String
removeTrailingSlash s =
    if String.startsWith "/" s then
        String.dropLeft 1 s
    else
        s