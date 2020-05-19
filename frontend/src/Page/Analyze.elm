module Page.Analyze exposing (Model, Msg, init, toSession, update, view)

import Session exposing (Session(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Page
import Route exposing (Route)
import Error
import Http
import RemoteData exposing (RemoteData(..))
import Task exposing (Task)
import App exposing (App)
import Agave exposing (Job)
import PlanB



---- MODEL ----


type alias Model =
    { session : Session
    , apps : RemoteData Http.Error (List App)
    , jobs : RemoteData Http.Error (List Job)
    , tab : String
    , query : String
    }


init : Session -> Maybe String -> ( Model, Cmd Msg )
init session tab =
    let
        (jobs, getJobs) =
            case session of
                LoggedIn _ _ _ cred ->
                    ( Loading
                    , Task.map2 Tuple.pair
                        (Agave.getJobs cred.token |> Http.toTask |> Task.map .result)
                        (PlanB.getJobs cred.token |> Http.toTask |> Task.map .result)
                            |> Task.attempt GetJobsCompleted
                    )

                _ ->
                    ( NotAsked, Cmd.none )
    in
    ( { session = session
      , apps = Loading
      , jobs = jobs
      , tab = tab |> Maybe.withDefault "Apps"
      , query = ""
      }
      , Cmd.batch
          [ App.fetchAll |> Http.send GetAppsCompleted
          , getJobs
          ]
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE --


type Msg
    = GetAppsCompleted (Result Http.Error (List App))
    | GetJobsCompleted (Result Http.Error (List Job, List Job))
    | SetTab String
    | SetQuery String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetAppsCompleted result ->
            ( { model | apps = RemoteData.fromResult result }, Cmd.none )

        GetJobsCompleted (Ok (agaveJobs, planbJobs)) ->
            ( { model | jobs = Success (agaveJobs ++ planbJobs) }, Cmd.none )

        GetJobsCompleted (Err error) ->
            ( { model | jobs = Failure error }, Cmd.none )

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
                Loading ->
                    ( 0, Page.viewSpinner )

                Success apps ->
                    ( List.length apps
                    , apps |> filterApp model.query |> viewApps
                    )

                Failure error ->
                    ( 0, Error.view error False )

                NotAsked ->
                    ( 0, text "" )

        ( numJobs, jobsRow ) =
            case model.jobs of
                Loading ->
                    ( 0, Page.viewSpinner )

                Success jobs  ->
                    ( List.length jobs
                    , jobs |> filterJob model.query |> viewJobs
                    )

                Failure error ->
                    ( 0, Error.view error False )

                NotAsked ->
                    ( 0
                    , div [ class "mt-2 ml-3 alert alert-secondary" ]
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
        , if model.tab == "Jobs" then
            jobsRow
          else
            appsRow
        ]


viewApps : List App -> Html Msg
viewApps apps =
    let
        row app =
            tr []
                [ td [] [ a [ Route.href (Route.App (String.fromInt app.id))] [ text app.name ] ]
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
                , td [] [ text job.app_id ]
                , td [ class "text-nowrap" ] [ text job.created ]
                , td [ class "text-nowrap" ] [ text job.ended ]
                , td [ class "text-nowrap" ] [ Page.viewJobStatus job.status ]
                ]

        sortByTimeDesc a b =
            case compare a.created b.created of
              LT -> GT
              EQ -> EQ
              GT -> LT
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
            (jobs |> List.sortWith sortByTimeDesc |> List.map row)
        ]


filterApp : String -> List { a | name : String } -> List { a | name : String }
filterApp query list =
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


filterJob : String -> List { a | name : String, app_id : String, status : String } -> List { a | name : String, app_id : String, status : String }
filterJob query list =
    let
        lowerQuery =
            String.toLower query

        filter item =
            String.contains lowerQuery (String.toLower item.name) ||
            String.contains lowerQuery (String.toLower item.app_id) ||
            String.contains lowerQuery (String.toLower item.status)
    in
    if query /= "" then
        List.filter filter list
    else
        list