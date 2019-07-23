port module Session exposing (..)

--import Data.Cart as Cart exposing (Cart)
import User as User exposing (User)
import Browser.Navigation
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra exposing (maybe)


-- IMPORTANT!!!
-- When changing the structure of this record be sure to change the version below to prevent
-- errors when decoding the old cookies (can manifest as infinite login loop)
type alias Session =
--    { cart : Cart
    { token : String
    , refreshToken : String
    , expiresIn : Maybe Int
--    , expiresAt : Maybe Int
    , user : Maybe User
--    , url : String
    , navKey : Maybe Browser.Navigation.Key
    , storageKey : String
    }


version : String
version =
    "0.0.1"


default : Session
default =
--    { cart = Cart.empty
    { token = ""
    , refreshToken = ""
    , expiresIn = Nothing
--    , expiresAt = Nothing
    , user = Nothing
--    , url = ""
    , navKey = Nothing
    , storageKey = "planetmicrobe" ++ version
    }


--expired : Session -> Session
--expired session =
--    { session
--        | expiresAt = Nothing
--        , expiresIn = Nothing
--        , token = ""
----        , user = Nothing
--    }


--isLoggedIn : Session -> Bool
--isLoggedIn session =
--    session.token /= ""


--decoder : Decoder Session
--decoder =
--    Decode.succeed Session
----        |> required "cart" Cart.decoder
--        |> optional "token" Decode.string ""
--        |> optional "refreshToken" Decode.string ""
--        |> optional "expiresIn" (Decode.nullable Decode.int) Nothing
--        |> optional "expiresAt" (Decode.nullable Decode.int) Nothing
----        |> optional "user" (Decode.nullable User.decoder) Nothing
--        |> optional "url" Decode.string ""


encode : Session -> Value
encode session =
    Encode.object
--        [ ("cart", Cart.encode session.cart)
        [ ("token", Encode.string session.token)
        , ("refreshToken", Encode.string session.refreshToken)
        , ("expiresIn", maybe Encode.int session.expiresIn)
--        , ("expiresAt", maybe Encode.int session.expiresAt)
        , ("user", maybe User.encodeUser session.user)
--        , ("url", Encode.string session.url)
        , ("storageKey", Encode.string session.storageKey)
        ]


store : Session -> Cmd msg
store session =
    encode session
        |> Just
        |> storeSession


port storeSession : Maybe Value -> Cmd msg


port onSessionChange : (String -> msg) -> Sub msg
