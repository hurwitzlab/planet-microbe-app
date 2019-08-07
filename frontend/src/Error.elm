module Error exposing (..)

import Http exposing (Error(..))
import Json.Decode as Json
import Browser.Navigation
import Route



parse : String -> Maybe String
parse =
    Json.decodeString (Json.field "error" Json.string) >> Result.toMaybe


toString : Http.Error -> String
toString err =
    case err of
        Timeout ->
            "Timeout exceeded"

        NetworkError ->
            "Network error"

        BadStatus resp ->
            parse resp.body
                |> Maybe.withDefault (String.fromInt resp.status.code ++ " " ++ resp.status.message)

        BadPayload text resp ->
            "Unexpected response from api: " ++ text

        BadUrl url ->
            "Malformed url: " ++ url


redirectLoadError : Error -> Browser.Navigation.Key -> Cmd msg
redirectLoadError error key =
    case error of
        Http.BadStatus response ->
            case response.status.code of
                401 ->
                    Route.replaceUrl key Route.Login -- redirect to Login page

                _ ->
                    Cmd.none

        _ -> Cmd.none