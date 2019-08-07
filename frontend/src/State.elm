port module State exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)



-- IMPORTANT!!!
-- When changing the structure of this record be sure to change the cookie name to prevent
-- errors when decoding the old cookies (can manifest as infinite login loop)
type alias State =
    { url : String
    , randomCode : String
    }


toString : State -> String
toString state =
    state.url ++ "." ++ state.randomCode


fromString : String -> Maybe State
fromString str =
--    let
--        randomCode =
--            str
--                |> stringDropLeftUntil (\c -> c == ".")
--    in
    case String.split "." str of
        [url, randomCode] ->
            Just (State url randomCode)

        _ ->
            Nothing


--stringDropLeftUntil : (String -> Bool) -> String -> String
--stringDropLeftUntil predicate str =
--    let
--        ( h, q ) =
--            ( String.left 1 str, String.dropLeft 1 str )
--    in
--    if q == "" || predicate h then
--        q
--    else
--        stringDropLeftUntil predicate q


default : State
default =
    { url = ""
    , randomCode = ""
    }


decoder : Decoder State
decoder =
    Decode.succeed State
        |> optional "url" Decode.string ""
        |> optional "randomCode" Decode.string ""


encode : State -> Value
encode state =
    Encode.object
        [ ("url", Encode.string state.url)
        , ("randomCode", Encode.string state.randomCode)
        ]


store : State -> Cmd msg
store state =
    encode state
        |> Just
        |> storeState


port storeState : Maybe Value -> Cmd msg


port onTrackerChange : (String -> msg) -> Sub msg
