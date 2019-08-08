module Page.Analyze exposing (Model, Msg, init, toSession, update, view)

import Session exposing (Session(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
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
    , query : String
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
      , query = ""
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
    | SetQuery String


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

        SetQuery query ->
            ( { model | query = query }, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    let
        ( numApps, appsRow ) =
            case model.apps of
                Just apps ->
                    ( List.length apps
                    , apps |> filterByName model.query |> viewApps
                    )

                Nothing ->
                    (0, Page.viewSpinner )

        ( numJobs, jobsRow ) =
            case model.jobs of
                Loading ->
                    ( 0, Page.viewSpinner )

                Loaded jobs  ->
                    ( List.length jobs
                    , jobs |> filterByName model.query |> viewJobs
                    )

                _ ->
                    ( 0
                    , div [ class "mt-2 ml-3" ]
                        [ a [ Route.href Route.Login ] [ text "Sign-in" ]
                        , text " to see your jobs"
                        ]
                    )

        navItem label count =
            li [ class "nav-item" ]
                [ a [ class "nav-link", classList [ ("border rounded", model.tab == label) ], href "", onClick (SetTab label) ]
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
        [ ul [ class "nav nav-justified mt-5 mb-4 h5" ]
            [ navItem "Apps" numApps
            , navItem "Jobs" numJobs
            , span [ class "w-75" ] [ input [ class "float-right", placeholder "Search", onInput SetQuery ] [] ]
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


filterByName : String -> List { a | name : String } -> List { a | name : String }
filterByName query list =
    let
        lowerQuery =
            String.toLower query

        filter item =
            String.contains lowerQuery (String.toLower item.name)
    in
    if query /= "" then
        List.filter filter list
    else
        list