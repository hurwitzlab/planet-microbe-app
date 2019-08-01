module Page.Job exposing (Model, Msg(..), init, toSession, update, view)

import Session exposing (Session)
import Agave exposing (Job)
import App exposing (App)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Route
import Page
import Task exposing (Task)
import Dict exposing (Dict)
import Time
--import View.Spinner exposing (spinner)
import FileBrowser
import Error
--import Debug exposing (toString)



---- MODEL ----


type alias Model =
    { session : Session
    , username : String --FIXME use session instead
    , jobId : String
    , job : Maybe Job
    , app : Maybe App
    , loadingJob : Bool
    , loadingHistory : Bool
    , loadedHistory : Bool
    , history : List Agave.JobHistory
    , loadingResults : Bool
    , loadedResults : Bool
    , results : Maybe (List (String, String))
    , startTime : Int -- milliseconds
    , lastPollTime : Int -- milliseconds
    , showCancelDialog : Bool
    , cancelDialogMessage : Maybe String
    , fileBrowser : Maybe FileBrowser.Model
    }


init : Session -> String -> ( Model, Cmd Msg )
init session id =
    let
--        _ = Debug.log "Job.init session:" (toString session)

        loadJobFromAgave =
            Agave.getJob (Session.token session) id |> Http.toTask |> Task.map .result

--        loadJobFromPlanB =
--            PlanB.getJob session.token id |> Http.toTask |> Task.map .result

        loadJob =
--            if String.startsWith "planb" id then
--                loadJobFromPlanB
--            else
                loadJobFromAgave

        loadApp app_name =
            App.fetchByName app_name |> Http.toTask

        username =
            Session.getUser session |> Maybe.map .user_name |> Maybe.withDefault ""
    in
    ( { session = session
      , username = username
      , jobId = id
      , job = Nothing
      , app = Nothing
      , loadingJob = False
      , loadingHistory = False
      , loadedHistory = False
      , history = []
      , loadingResults = False
      , loadedResults = False
      , results = Nothing
      , startTime = 0
      , lastPollTime = 0
      , showCancelDialog = False
      , cancelDialogMessage = Nothing
      , fileBrowser = Nothing
      }
      , loadJob
            |> Task.andThen
                (\job ->
                    (loadApp job.app_id)
                        |> Task.andThen
                            (\app ->
                                Task.succeed (job, app)
                            )
                )
            |> Task.attempt GetJobCompleted
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE --


type Msg
    = GetJobCompleted (Result Http.Error (Job, App))
    | GetHistory
    | SetHistory (List Agave.JobHistory)
    | ShowOutputs
    | GetResults
    | SetResults (Result Http.Error (List (String, String)))
    | SetJob Agave.Job
    | PollJob Time.Posix
    | CancelJob
    | CancelJobCompleted (Result Http.Error Agave.Job)
    | CloseCancelDialog
    | FileBrowserMsg FileBrowser.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        username =
            Session.getUser model.session |> Maybe.map .user_name |> Maybe.withDefault ""

        loadJobFromAgave =
            Agave.getJob (Session.token model.session) model.jobId |> Http.toTask |> Task.map .result

--        loadJobFromPlanB =
--            PlanB.getJob model.session.token model.jobId |> Http.toTask |> Task.map .result

        loadJob =
--            if String.startsWith "planb" model.job_id then
--                loadJobFromPlanB
--            else
                loadJobFromAgave
    in
    case msg of
        GetJobCompleted (Ok (job, app)) ->
            ( { model
                | job = Just job
                , app = Just app
              }
            , Cmd.none
            )

        GetJobCompleted (Err error) -> --TODO
--            let
--                _ = Debug.log "GetJobCompleted" (toString error)
--            in
            ( model, Cmd.none )

        GetHistory ->
            let
                loadHistoryFromAgave =
                    Agave.getJobHistory (Session.token model.session) model.jobId |> Http.toTask |> Task.map .result

--                loadHistoryFromPlanB =
--                    PlanB.getJobHistory model.session.token model.jobId |> Http.toTask |> Task.map .result

                loadHistory =
--                    if String.startsWith "planb" model.jobId then
--                        loadHistoryFromPlanB
--                    else
                       loadHistoryFromAgave

                handleHistory history =
                    case history of
                        Ok h ->
                            SetHistory h

                        Err _ ->
--                            let
--                                _ = Debug.log "Error" "could not retrieve job history"
--                            in
                            SetHistory []
            in
            ( { model | loadingHistory = True }, Task.attempt handleHistory loadHistory )

        SetHistory history ->
--            let
--                filtered =
--                    List.filter (\output -> output.name /= ".") outputs
--            in
            ( { model | history = history, loadingHistory = False, loadedHistory = True }, Cmd.none )

        ShowOutputs ->
            let
                defaultConfig =
                    FileBrowser.defaultConfig

                outputsPath =
                    ( model.job |> Maybe.map .owner |> Maybe.withDefault "" ) ++ ( "/archive/jobs/job-" ++ model.jobId )

                fileBrowser =
                    FileBrowser.init model.session (Just { defaultConfig | showMenuBar = False, homePath = Just outputsPath })

                (subModel, subCmd) =
                    FileBrowser.update model.session FileBrowser.RefreshPath fileBrowser
            in
            ( { model | fileBrowser = Just subModel }, Cmd.map FileBrowserMsg subCmd )

        GetResults -> -- this code is a little complicated
            let
                owner =
                    model.job |> Maybe.map .owner |> Maybe.withDefault ""

                loadOutputs path =
                    Agave.getJobOutputs owner (Session.token model.session) model.jobId (Just path)
                        |> Http.toTask
                        |> Task.map .result
                        |> Task.map (List.filter (\r -> r.name /= "." && String.endsWith ".tab" r.name) >> List.map .path) -- filter out current path "." #FIXME hardcoded for .tab files (for ohana-blast) 

                -- Expects relative path
                loadOutput path =
                    Agave.getJobOutput owner (Session.token model.session) model.jobId path
                        |> Http.toTask |> Task.map (\data -> List.singleton (path, data))

                -- Expects full path
                loadFile path =
                    Agave.getFile (Session.token model.session) path
                        |> Http.toTask |> Task.map (\data -> List.singleton (path, data))

                -- Gets a single file or every file in a directory if path ends in "/"
                loadResultData path =
                    case String.endsWith "/" path of
                        False ->
                            loadOutput path

                        True ->
                            -- Get contents of every file in the path
                            loadOutputs path
                                |> Task.andThen
                                    (\outputs -> outputs |> List.map loadFile |> Task.sequence |> Task.map List.concat)

--                loadResults =
--                    model.app.app_results |> List.map (loadResultData << .path) |> Task.sequence |> Task.map List.concat
            in
            ( { model | loadingResults = True }
            , Cmd.none --Task.attempt SetResults loadResults
            )

        SetResults (Ok results) ->
--            case results of
--                [] ->
--                    ( { model | loadedResults = True }, Cmd.none ) -- File not found
--
--                _ ->
--                    let
--                        datasets =
--                            List.Extra.lift2 (\a b -> (a.app_data_type.name, Tuple.first b, Tuple.second b)) model.app.app_results results
--                            -- TODO change createSimPlot port to accept record instead of list of tuples
--                    in
--                    ( { model | loadedResults = True, results = Just results }
--                    , Ports.createSimPlot ("sim-plot", datasets)
--                    )
            ( model, Cmd.none )

        SetResults (Err error) ->
--            let
--                _ = Debug.log "SetResults" ("Error retrieving results: " ++ (toString error))
--            in
            ( { model | loadedResults = True }, Cmd.none )

        SetJob job ->
            ( { model | job = Just job, loadingJob = False }, Cmd.none )

        PollJob time ->
            case model.job of
                Just job ->
                    if model.loadingJob == False && isRunning job then
                        let
--                            _ = Debug.log "Job.Poll" ("polling job " ++ job.id)

                            startTime =
                                if model.startTime == 0 then
                                    Time.posixToMillis time
                                else
                                    model.startTime

                            lastPollTime =
                                if model.lastPollTime == 0 then
                                    Time.posixToMillis time
                                else
                                    model.lastPollTime

                            timeSinceStart =
                                Time.posixToMillis time - startTime

                            timeSinceLastPoll =
                                Time.posixToMillis time - lastPollTime

                            handleJob job_ =
                                case job_ of
                                    Ok j ->
                                        SetJob j

                                    Err error ->
--                                        let
--                                            _ = Debug.log "Error" ("could not poll job" ++ (errorString error))
--                                        in
                                        SetJob job

                            second =
                                1000

                            minute =
                                60 * second

                            doPoll =
                                -- Poll every 10 seconds if job has been running less than 15 minutes
                                if timeSinceStart < (15 * minute) && timeSinceLastPoll >= (10 * second) then
                                    True
                                -- Poll every 30 seconds if job has been running less than 30 minutes
                                else if timeSinceStart < (30 * minute) && timeSinceLastPoll >= (30 * second) then
                                    True
                                -- Poll every 60 seconds if job has been running longer than 30 minutes
                                else if timeSinceStart >= (30 * minute) && timeSinceLastPoll >= (60 * second) then
                                    True
                                else
                                    False

                            newModel =
                                { model | startTime = startTime, lastPollTime = Time.posixToMillis time }
                        in
                        case doPoll of
                            True ->
                                ( { newModel | loadingJob = True }, Task.attempt handleJob loadJob )

                            False ->
                                ( newModel, Cmd.none )
                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        CancelJob ->
            let
                stopJob =
                    Agave.stopJob (Session.token model.session) model.jobId
                        |> Http.toTask
                        |> Task.andThen (\_ -> loadJob)
            in
            ( { model | showCancelDialog = True, cancelDialogMessage = Nothing }, Task.attempt CancelJobCompleted stopJob )

        CancelJobCompleted (Ok job) ->
            let
                errorMsg =
                    "A cancellation request was sent.  This may or may not result in the termination of the job depending on its state."
            in
            ( { model | cancelDialogMessage = Just errorMsg, job = Just job }, Cmd.none )

        CancelJobCompleted (Err error) ->
            ( { model | cancelDialogMessage = Just (Error.toString error) }, Cmd.none )

        CloseCancelDialog ->
            ( { model | showCancelDialog = False }, Cmd.none )

        FileBrowserMsg subMsg ->
            case model.fileBrowser of
                Nothing ->
                    ( model, Cmd.none )

                Just fileBrowser ->
                    let
                        ( newFileBrowser, subCmd ) =
                            FileBrowser.update model.session subMsg fileBrowser
                    in
                    ( { model | fileBrowser = Just newFileBrowser }, Cmd.map FileBrowserMsg subCmd )



-- VIEW --


view : Model -> Html Msg
view model =
    case ( model.job, model.app ) of
        ( Just job, Just app ) ->
            let
                status =
                    job.status |> String.replace "_" " "  -- replace _ with space

                deUrl =
                    "https://de.cyverse.org/de/?type=data&folder=/iplant/home/" ++ model.username ++ "/archive/jobs/job-" ++ model.jobId --FIXME move base url to config
            in
            div [ class "container" ]
                [ Page.viewTitle "Job" job.name
                , viewJob job app
                , br [] []
                , Page.viewTitle2 "Inputs" False
                , viewInputs job.inputs
                , br [] []
                , Page.viewTitle2 "Parameters" False
                , viewParameters job.parameters
                , br [] []
                , Page.viewTitle2 "History" False
                , viewHistory model.history model.loadedHistory model.loadingHistory
                , br [] []
                , div [ class "mt-4 border-bottom", style "width" "100%" ]
                    [ h2 [ class "font-weight-bold d-inline" ]
                        [ span [ style "color" "dimgray" ] [ text "Outputs" ]
                        ]
                    , span [ class "float-right pt-2" ]
                        [ text "View output files in the "
                        , a [ target "_blank", href deUrl ]
                            [ text "CyVerse Data Store "
                            , i [ class "fas fa-external-link-alt fa-xs align-baseline" ] []
                            ]
                        ]
                    ]
                , viewOutputs model
--                , Page.viewTitle2 "Results" False
--                , viewResults model
                ]
    --        , Dialog.view
    --            (if model.showCancelDialog then
    --                Just (cancelDialogConfig model)
    --            else
    --                Nothing
    --            )

        ( _, _ ) ->
            text ""


viewJob : Job -> App -> Html Msg
viewJob job app =
    table [ class "table table-borderless table-sm" ]
        [ tr []
            [ th [] [ text "ID" ]
            , td [] [ text job.id ]
            ]
        , tr []
            [ th [] [ text "Name" ]
            , td [] [ text job.name ]
            ]
        , tr []
            [ th [] [ text "App" ]
            , td [] [ a [ Route.href (Route.App app.id) ] [ text app.name ] ]
            ]
        , tr []
            [ th [] [ text "Owner" ]
            , td [] [ text job.owner ]
            ]
        , tr []
            [ th [] [ text "Start Time" ]
            , td [] [ text job.created ]
            ]
        , tr []
            [ th [] [ text "End Time" ]
            , td [] [ text job.ended ]
            ]
        , tr []
            [ th [ class "top" ] [ text "Status" ]
            , td []
                [ viewStatus job.status
                , if isRunning job then
                    button [ class "btn btn-outline-secondary btn-sm ml-2 align-top", onClick CancelJob ] [ text "Cancel" ]
                 else
                    text ""
                ]
            , td [] []
            ]
        ]


isRunning : Agave.Job -> Bool
isRunning job =
    job.status /= "FINISHED" && job.status /= "FAILED" && job.status /= "STOPPED"


viewStatus : String -> Html msg
viewStatus status =
    let
        progressBar pct =
            let
                label =
                    String.replace "_" " " status -- replace _ with space
            in
            div [ class "progress float-left d-inline-block", style "width" "20em", style "height" "2.5em" ]
                [ div [ class "progress-bar progress-bar-striped active", style "width" ((String.fromInt pct) ++ "%"), style "height" "2.5em",
                        attribute "role" "progressbar", attribute "aria-valuenow" (String.fromInt pct), attribute "aria-valuemin" "0", attribute "aria-valuemax" "100" ]
                    [ text label ]
                ]
    in
    case String.toUpper status of
        "CREATED" -> progressBar 10
        "ACCEPTED" -> progressBar 10
        "PENDING" -> progressBar 20
        "PROCESSING_INPUTS" -> progressBar 30
        "STAGING_INPUTS" -> progressBar 40
        "STAGED" -> progressBar 45
        "STAGING_JOB" -> progressBar 50
        "SUBMITTING" -> progressBar 55
        "QUEUED" -> progressBar 60
        "RUNNING" -> progressBar 70
        "CLEANING_UP" -> progressBar 80
        "ARCHIVING" -> progressBar 90
        _ -> text status


viewInputs : Dict String Agave.JobInputValue -> Html msg
viewInputs inputs =
    table [ class "table" ]
        [ tbody []
            (if Dict.size inputs == 0 then
                [ tr [] [ td [] [ text "None" ] ] ]
            else
                Dict.toList inputs |> List.map viewInput
            )
        ]


viewInput : (String, Agave.JobInputValue) -> Html msg
viewInput (id, val) =
    let
        valList =
            case val of
                Agave.JobInputString s ->
                    [ s ]

                Agave.JobInputArray a ->
                    a
    in
    tr []
        [ th [] [ text id ]
        , td [] [ text (String.join "; " valList) ]
        ]


viewParameters : Dict String Agave.ValueType -> Html msg
viewParameters params =
    table [ class "table" ]
        [ tbody []
            (if Dict.size params == 0 then
                [ tr [] [ td [] [ text "None" ] ] ]
             else
                Dict.toList params |> List.map viewParameter
            )
        ]


viewParameter : (String, Agave.ValueType) -> Html msg
viewParameter (id, value) =
    let
        valueStr =
            case value of --TODO move to separate function
                Agave.StringValue s ->
                    s

                Agave.BoolValue b ->
                    if b then
                        "True"
                    else
                        "False"

                Agave.NumberValue n ->
                    String.fromFloat n

                Agave.ArrayValue l ->
                    String.join ";" l
    in
    tr []
        [ th [] [ text id ]
        , td [] [ text valueStr ]
        ]


viewHistory : List Agave.JobHistory -> Bool -> Bool -> Html Msg
viewHistory history loaded loading =
    if history == [] then
        div [ class "border-top w-100 pt-2" ]
            [ if loaded then
                text "None"
              else if loading then
                text "Loading..." --spinner
              else
                button [ class "btn btn-outline-secondary", onClick GetHistory ] [ text "Show History" ]
            ]
    else
    table [ class "table table-sm" ]
        [ tbody []
            (List.map viewEvent history)
        ]


viewEvent : Agave.JobHistory -> Html msg
viewEvent event =
    tr []
        [ td [ class "text-nowrap" ] [ text event.created ]
        , td [ class "text-nowrap" ] [ text event.status ]
        , td [] [ text event.description ]
        ]


viewOutputs : Model -> Html Msg
viewOutputs model =
    let
        body =
            case model.job of
                Just job ->
                    case job.status of
                        "FINISHED" ->
                            case model.fileBrowser of
                                Nothing ->
                                    div [ class "mt-2" ]
                                        [ button [ class "btn btn-outline-secondary", onClick ShowOutputs ] [ text "Show Outputs" ] ]

                                Just fileBrowser ->
                                    div [ style "height" "60vh", style "overflow-y" "auto" ]
                                        [ FileBrowser.view fileBrowser |> Html.map FileBrowserMsg ]

                        "FAILED" ->
                            text "None"

                        _ ->
                            text "Job is not FINISHED, please wait ..."

                Nothing ->
                    text ""
    in
    div []
        [ body ]


viewResults : Model -> Html Msg
viewResults model =
    let
        body =
            case model.job of
                Just job ->
                    case job.status of
                        "FINISHED" ->
                            case model.loadedResults of
                                True ->
                                    case model.results of
                                        Nothing ->
                                            text "None"

                                        _ ->
                                            div [] []

                                False ->
                                    case model.loadingResults of
                                        True ->
                                            text "Loading..." --spinner

                                        False ->
                                            button [ class "btn btn-default", onClick GetResults ] [ text "Show Results" ]

                        "FAILED" ->
                            tr [] [ td [] [ text "None" ] ]

                        _ ->
                            div [ class "italic" ] [ text "Job is not FINISHED, please wait ..." ]

                Nothing ->
                    text ""
    in
    div []
        [ h2 [] [ text "Results" ]
        , table [ class "table" ]
            [ tbody []
                [ tr []
                    [ td []
                        [ div [] [ body ]
                        , div [ id "sim-plot" ] [] -- has to be located here for accessibility from heatmap.js
                        ]
                    ]
                ]
            ]
        ]


--cancelDialogConfig : Model -> Dialog.Config Msg
--cancelDialogConfig model =
--    let
--        content =
--            case model.cancelDialogMessage of
--                Nothing ->
--                    text "Loading..." --spinner
--
--                Just message ->
--                    div [ class "alert alert-info" ]
--                        [ p [] [ text message ]
--                        ]
--
--        footer =
--            if model.cancelDialogMessage == Nothing then
--                div [] [ text " " ]
--            else
--                button [ class "btn btn-default", onClick CloseCancelDialog ] [ text "OK" ]
--    in
--    { closeMessage = Nothing
--    , containerClass = Nothing
--    , header = Just (h3 [] [ text "Cancel Job" ])
--    , body = Just content
--    , footer = Just footer
--    }
