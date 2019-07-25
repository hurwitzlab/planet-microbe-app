module Page.Account exposing (Model, Msg, init, toSession, update, view)

import Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Page
import Route
import Experiment exposing (Experiment)
import Run exposing (Run)
import Http
--import Page.Error as Error exposing (PageLoadError)
import Task exposing (Task)
import String.Extra
import Json.Encode as Encode
--import Debug exposing (toString)



---- MODEL ----


type alias Model =
    { session : Session
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      }
    , Cmd.none
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE --


type Msg
    = TODO


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TODO ->
            ( model, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    let
        username =
            model.session.user |> Maybe.map .user_name |> Maybe.withDefault ""

        firstName =
            model.session.user |> Maybe.map .first_name |> Maybe.withDefault ""

        lastName =
            model.session.user |> Maybe.map .last_name |> Maybe.withDefault ""
    in
    div [ class "container" ]
        [ div [ class "pb-2 mt-5 mb-2 border-bottom", style "width" "100%" ]
            [ h1 [ class "font-weight-bold d-inline", style "color" "dimgray" ] [ text "Account" ]
            , a [ class "btn btn-primary float-right", Route.href (Route.Logout) ] [ text "Sign-out" ]
            ]
        , table [ class "table table-borderless table-sm" ]
            [ tr []
                [ th [ class "w-25" ] [ text "Username" ]
                , td [] [ text username ]
                ]
            , tr []
                [ th [] [ text "Full Name" ]
                , td [] [ text (firstName ++ " " ++ lastName) ]
                ]
            ]
        , div [ class "alert alert-info mt-5" ]
            [ p []
                [ text "The user information shown above was obtained from your "
                , a [ href "http://www.cyverse.org/", target "_blank" ] [ text "CyVerse" ]
                , text " account."
                ]
            , p []
                [ text "For details please see the "
                , a [ href "https://user.cyverse.org/", target "_blank" ] [ text "CyVerse User Portal" ]
                , text "."
                ]
            ]
        ]