module User exposing (..)

import Http
import HttpBuilder
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)
import Config exposing (apiBaseUrl)



-- TYPES --


-- IMPORTANT!!!
-- This type exists in the Session.
-- When changing the structure of this record be sure to change the cookie name
-- to prevent errors when decoding the old cookies (can manifest as infinite login loop)
type alias User =
    { user_id : Int
    , user_name : String
    , first_name : String
    , last_name : String
    , email : String
--    , date : String
--    , orcid : String
--    , role : Int
--    , projects : List Project
--    , project_groups : List ProjectGroup
--    , log : List LogEntry
    }



-- SERIALIZATION --


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "user_id" Decode.int
        |> required "user_name" Decode.string
        |> required "first_name" Decode.string
        |> required "last_name" Decode.string
        |> optional "email" Decode.string ""
--        |> required "date" Decode.string
--        |> optional "orcid" Decode.string ""
--        |> optional "role" Decode.int 0
--        |> optional "projects" (Decode.list decoderProject) []
--        |> optional "project_groups" (Decode.list decoderProjectGroup) []
--        |> optional "log" (Decode.list decoderLogEntry) []


encodeUser : User -> Value
encodeUser user =
    Encode.object
        [ ( "user_id", Encode.int user.user_id )
        , ( "user_name", Encode.string user.user_name )
        , ( "first_name", Encode.string user.first_name )
        , ( "last_name", Encode.string user.last_name )
        , ( "email", Encode.string user.email )
--        , ( "date", Encode.string user.date )
--        , ( "orcid", Encode.string user.orcid )
--        , ( "role", Encode.int user.role )
        ]



-- REQUESTS --


recordLogin : String -> Http.Request User
recordLogin token =
    let
        url =
            apiBaseUrl ++ "/users/login"

        headers =
            [( "Authorization", "Bearer " ++ token)]
    in
    HttpBuilder.post url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson userDecoder)
        |> HttpBuilder.toRequest



-- UTILITY FUNCTIONS --


--isBetaUser : Maybe User -> Bool
--isBetaUser user =
--    (user |> Maybe.map .role |> Maybe.withDefault 0) > 0
--
--
--isAdminUser : Maybe User -> Bool
--isAdminUser user =
--    (user |> Maybe.map .role |> Maybe.withDefault 0) >= 127
