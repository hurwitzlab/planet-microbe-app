module Page.Analyze exposing (Model, Msg, init, toSession, update, view)

import Session exposing (Session(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Page
import Route exposing (Route)
import Http
import Task exposing (Task)
import App exposing (App)
import Agave exposing (Job)
--import Debug exposing (toString)



---- MODEL ----


type alias Model =
    { session : Session
    , apps : Maybe (List App)
    , jobs : Jobs
    , tab : String
    }


type Jobs
    = Unavailable
    | Loading
    | Loaded (List Job)
    | LoadError


init : Session -> ( Model, Cmd Msg )
init session =
    let
        (jobs, getJobs) =
            case session of
                LoggedIn _ _ _ cred ->
                    ( Loading, [ Agave.getJobs cred.token |> Http.toTask |> Task.map .result |> Task.attempt GetJobsCompleted ] )

                _ ->
                    ( Unavailable, [] )
    in
    ( { session = session
      , apps = Nothing
      , jobs = jobs
      , tab = "Apps"
      }
      , Cmd.batch ( (App.fetchAll |> Http.toTask |> Task.attempt GetAppsCompleted) :: getJobs )
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE --


type Msg
    = GetAppsCompleted (Result Http.Error (List App))
    | GetJobsCompleted (Result Http.Error (List Job))
    | SetTab String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetAppsCompleted (Ok apps) ->
            ( { model | apps = Just apps }, Cmd.none )

        GetAppsCompleted (Err error) -> --TODO
--            let
--                _ = Debug.log "GetAppsCompleted" (toString error)
--            in
            ( model, Cmd.none )

        GetJobsCompleted (Ok jobs) ->
            ( { model | jobs = Loaded jobs }, Cmd.none )

        GetJobsCompleted (Err error) -> --TODO
--            let
--                _ = Debug.log "GetJobsCompleted" (toString error)
--            in
            ( { model | jobs = LoadError } , Cmd.none )

        SetTab label ->
            ( { model | tab = label }, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    let
        numApps =
            model.apps |> Maybe.withDefault [] |> List.length

        appsRow =
            case model.apps of
                Just apps ->
                    viewApps apps

                Nothing ->
                    Page.viewSpinner

        ( numJobs, jobsRow ) =
            case model.jobs of
                Unavailable ->
                    ( 0
                    , div [ class "mt-2 ml-3" ]
                        [ a [ Route.href Route.Login ] [ text "Sign-in" ]
                        , text " to see your jobs"
                        ]
                    )

                Loading ->
                    ( 0, Page.viewSpinner )

                Loaded jobs  ->
                    ( List.length jobs
                    , div [ style "overflow-y" "auto", style "max-height" "80vh" ]
                        [ viewJobs jobs ]
                    )

                LoadError ->
                    ( 0, div [ class "mt-2 ml-3" ] [ text "An error occured" ] )

        navItem label count =
            li [ class "nav-item" ]
                [ a [ class "nav-link", classList [ ("active", model.tab == label) ], href "", onClick (SetTab label) ]
                    [ text label
                    , text " "
                    , if count > 0 then
                        span [ class "badge badge-light" ]
                            [ text (String.fromInt count) ]
                      else
                        text ""
                    ]
                ]
    in
    div [ class "container" ]
        [ ul [ class "nav nav-pills mt-5 mb-4" ]
            [ navItem "Apps" numApps
            , navItem "Jobs" numJobs
            ]
        , if model.tab == "Apps" then
            appsRow
          else
            jobsRow
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
            (apps |> List.sortBy (.name >> String.toLower) |> List.map row)
        ]


viewJobs : List Job -> Html Msg
viewJobs jobs =
    let
        row job =
            tr []
                [ td [] [ a [ Route.href (Route.Job job.id)] [ text job.name ] ]
                , td [] [ text "" ]
                , td [] [ text job.created ]
                , td [] [ text job.ended ]
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