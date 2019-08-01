module Session exposing (..)

import Cart exposing (Cart)
import Credentials exposing (Credentials)
import User as User exposing (User)
import Browser.Navigation as Nav
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra exposing (maybe)



type Session
    = LoggedIn Nav.Key Cart Credentials
    | Guest Nav.Key Cart


fromKey : Nav.Key -> Session
fromKey key =
    Guest key Cart.empty


navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn key _ _ ->
            key

        Guest key _ ->
            key


getCart : Session -> Cart
getCart session =
    case session of
        LoggedIn _ c _ ->
            c

        Guest _ c ->
            c


setCart : Session -> Cart -> Session
setCart session cart =
    case session of
        LoggedIn key _ cred ->
            LoggedIn key cart cred

        Guest key _ ->
            Guest key cart


credentials : Session -> Maybe Credentials
credentials session =
    case session of
        LoggedIn _ _ cred ->
            Just cred

        Guest _ _ ->
            Nothing


setCredentials : Session -> Credentials -> Session
setCredentials session cred =
    case session of
        LoggedIn key cart _ ->
            LoggedIn key cart cred

        Guest key cart ->
            LoggedIn key cart cred


token : Session -> String
token session =
    case session of
        LoggedIn _ _ cred ->
            cred.token

        Guest _ _ ->
            ""


getUser : Session -> Maybe User
getUser session =
    case session of
        LoggedIn _ _ cred ->
            cred.user

        Guest _ _ ->
            Nothing


setUser : Session -> User -> Session
setUser session user =
    let
        default =
            Credentials.default
    in
    case session of
        LoggedIn key cart cred ->
            LoggedIn key cart { cred | user = Just user }

        Guest key cart ->
            LoggedIn key cart { default | user = Just user }


logout : Session -> Session
logout session =
    case session of
        LoggedIn key cart _ ->
            Guest key cart

        _ ->
            session