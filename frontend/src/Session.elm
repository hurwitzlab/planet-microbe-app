module Session exposing (..)

import Cart exposing (Cart)
import Credentials exposing (Credentials)
import State exposing (State)
import User as User exposing (User)
import Browser.Navigation as Nav



type Session
    = LoggedIn Nav.Key State Cart Credentials
    | Guest Nav.Key State Cart


fromKey : Nav.Key -> Session
fromKey key =
    Guest key State.default Cart.empty


navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn key _ _ _ ->
            key

        Guest key _ _ ->
            key


getState : Session -> State
getState session =
     case session of
        LoggedIn _ state _ _ ->
            state

        Guest _ state _ ->
            state


setState : Session -> State -> Session
setState session state =
    case session of
        LoggedIn key _ cart cred ->
            LoggedIn key state cart cred

        Guest key _ cart ->
            Guest key state cart


getCart : Session -> Cart
getCart session =
    case session of
        LoggedIn _ _ c _ ->
            c

        Guest _ _ c ->
            c


setCart : Session -> Cart -> Session
setCart session cart =
    case session of
        LoggedIn key state _ cred ->
            LoggedIn key state cart cred

        Guest key state _ ->
            Guest key state cart


credentials : Session -> Maybe Credentials
credentials session =
    case session of
        LoggedIn _ _ _ cred ->
            Just cred

        Guest _ _ _ ->
            Nothing


setCredentials : Session -> Credentials -> Session
setCredentials session cred =
    case session of
        LoggedIn key state cart _ ->
            LoggedIn key state cart cred

        Guest key state cart ->
            LoggedIn key state cart cred


token : Session -> String
token session =
    case session of
        LoggedIn _ _ _ cred ->
            cred.token

        Guest _ _ _ ->
            ""


getUser : Session -> Maybe User
getUser session =
    case session of
        LoggedIn _ _ _ cred ->
            cred.user

        Guest _ _ _ ->
            Nothing


setUser : Session -> User -> Session
setUser session user =
    let
        default =
            Credentials.default
    in
    case session of
        LoggedIn key state cart cred ->
            LoggedIn key state cart { cred | user = Just user }

        Guest key state cart ->
            LoggedIn key state cart { default | user = Just user }


logout : Session -> Session
logout session =
    case session of
        LoggedIn key state cart _ ->
            Guest key state cart

        _ ->
            session