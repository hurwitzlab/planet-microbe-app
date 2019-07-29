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
import Agave exposing (Job)
import Debug exposing (toString)



---- MODEL ----


type alias Model =
    { session : Session
    , apps : Maybe (List App)
    , jobs : Maybe (List Job)
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , apps = Nothing
      , jobs = Nothing
      }
      , Cmd.batch
        [ App.fetchAll |> Http.toTask |> Task.attempt GetAppsCompleted
        , Agave.getJobs session.token |> Http.toTask |> Task.map .result |> Task.attempt GetJobsCompleted
        ]
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE --


type Msg
    = GetAppsCompleted (Result Http.Error (List App))
    | GetJobsCompleted (Result Http.Error (List Job))


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

        GetJobsCompleted (Ok jobs) ->
            ( { model | jobs = Just jobs }, Cmd.none )

        GetJobsCompleted (Err error) -> --TODO
            let
                _ = Debug.log "GetJobsCompleted" (toString error)
            in
            ( model, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    let
        appsRow =
            case model.apps of
                Just apps ->
                    let
                        numApps =
                            List.length apps
                    in
                    div []
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
                        ]

                Nothing ->
                    Page.viewSpinner

        jobsRow =
            case model.jobs of
                Just jobs  ->
                    let
                        numJobs =
                            List.length jobs
                    in
                    div []
                        [ div [ class "pt-3" ]
                            [ Page.viewTitle2 "Jobs" False
                            , span [ class "badge badge-pill badge-primary align-middle ml-2" ]
                                [ if numJobs == 0 then
                                    text ""
                                  else
                                    text (String.fromInt numJobs)
                                ]
                            ]
                        , div [ class "pt-2", style "overflow-y" "auto", style "max-height" "80vh" ]
                            [ viewJobs jobs ]
                        ]

                Nothing ->
                    Page.viewSpinner
    in
    div [ class "container" ]
        [ appsRow
        , jobsRow
        ]


viewApps : List App -> Html Msg
viewApps apps =
    let
        row app =
            tr []
                [ td [] [ a [ Route.href (Route.App app.id)] [ text app.name ] ]
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
            (apps |> List.map row)
        ]


viewJobs : List Job -> Html Msg
viewJobs jobs =
    let
        row job =
            tr []
                [ td [] [ a [ Route.href (Route.Job job.id)] [ text job.name ] ]
                , td [] [ text "" ]
                , td [] [ text "" ]
                , td [] [ text "" ]
                , td [] [ text job.status ]
                ]
    in
    table [ class "table" ]
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "App" ]
                , th [] [ text "Start" ]
                , th [] [ text "End" ]
                , th [] [ text "Status" ]
                ]
            ]
        , tbody []
            (jobs |> List.map row)
        ]