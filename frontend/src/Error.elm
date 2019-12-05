module Error exposing (..)

import Html exposing (Html, div, a, p, text)
import Html.Attributes exposing (class, href)
import Http exposing (Error(..))
import Json.Decode as Json
import Browser.Navigation
import Route



redirectLoadError : Error -> Browser.Navigation.Key -> Cmd msg
redirectLoadError error key =
    case error of
        Http.BadStatus response ->
            case response.status.code of
                401 ->
                    Route.replaceUrl key Route.Login -- redirect to Login page

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


view : Error -> Bool -> Html msg
view error isLoggedIn =
    let
        loginMessage =
            if isLoggedIn then
                text ""
            else
                p []
                    [ text "Please "
                    , a [ Route.href Route.Login ] [ text "sign-in"]
                    , text " and try again."
                    ]
    in
    div [ class "alert alert-danger m-5" ]
        (case error of
            Http.BadStatus response ->
                case response.status.code of
                    401 ->
                        [ p [] [ text "Access to this part of the site requires a CyVerse account." ]
                        , p []
                            [ text "If you do not have an account, please sign up at the "
                            , a [ href "https://user.cyverse.org/" ] [ text "CyVerse User Portal" ]
                            , text "."
                            ]
                        , p [] [ text "You will be redirected to the CyVerse login page in a few seconds ..." ]
                        ]

                    403 ->
                        [ p [] [ text "You do not have permission to access this resource." ]
                        , loginMessage
                        ]

                    _ ->
                        [ p [] [ text "An error occurred:" ]
                        , p [] [ text <| toString error ]
                        ]

            _ ->
                [ p [] [ text "An error occurred:" ]
                , p [] [ text <| toString error ]
                ]
        )


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


parse : String -> Maybe String
parse =
    Json.decodeString (Json.field "error" Json.string) >> Result.toMaybe