module Page.Analyze exposing (Model, Msg, init, toSession, update, view)

import Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page
import Route exposing (Route)
import Http
import Json.Encode as Encode
--import Page.Error as Error exposing (PageLoadError)
import Task exposing (Task)
import App exposing (App)
import Debug exposing (toString)



---- MODEL ----


type alias Model =
    { session : Session
    , apps : Maybe (List App)
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , apps = Nothing
      }
      , Cmd.batch
        [ App.fetchAll |> Http.toTask |> Task.attempt GetAppsCompleted
        ]
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE --


type Msg
    = GetAppsCompleted (Result Http.Error (List App))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetAppsCompleted (Ok apps) ->
            ( { model | apps = Just apps }, Cmd.none )

        GetAppsCompleted (Err error) -> --TODO
            let
                _ = Debug.log "GetAppsCompleted" (toString error)
            in
            ( model, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    case model.apps of
        Nothing ->
            text ""

        Just apps ->
            let
                numApps =
                    List.length apps

                numJobs =
                    0
            in
            div [ class "container" ]
                [ div [ class "pt-5" ]
                    [ Page.viewTitle2 "Apps" False
                    , span [ class "badge badge-pill badge-primary align-middle ml-2" ]
                        [ if numApps == 0 then
                            text ""
                          else
                            text (String.fromInt numApps)
                        ]
                    ]
                , div []
                    [ viewApps apps ]
                , div [ class "pt-3" ]
                    [ Page.viewTitle2 "Jobs" False
                    , span [ class "badge badge-pill badge-primary align-middle ml-2" ]
                        [ if numJobs == 0 then
                            text ""
                          else
                            text (String.fromInt numJobs)
                        ]
                    ]
                , div [ class "pt-2", style "overflow-y" "auto", style "max-height" "80vh" ]
                    [ text "None" ] --[ viewJobs jobs ]
                ]


viewApps : List App -> Html Msg
viewApps apps =
    let
        appRow app =
            tr []
                [ td [] [ text app.name ]
                , td [] [ text "" ]
                , td [] [ text "" ]
                ]
    in
    table [ class "table" ]
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Tags" ]
                , th [] [ text "Data Types" ]
                ]
            ]
        , tbody []
            (apps |> List.map appRow)
        ]
