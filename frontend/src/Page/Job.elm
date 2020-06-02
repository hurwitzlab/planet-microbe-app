port module Page.Job exposing (Model, Msg(..), init, toSession, subscriptions, update, view)

import Session exposing (Session)
import Agave exposing (Job, FileResult)
import PlanB
import App exposing (App)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Http
import RemoteData exposing (RemoteData(..))
import Route
import Page exposing (viewJobStatus)
import Task exposing (Task)
import Dict exposing (Dict)
import List.Extra
import Time
import FileBrowser
import Icon
import Error
import Config exposing (apiBaseUrl)



---- MODEL ----


type alias Model =
    { session : Session
    , jobId : String
    , job : Maybe Job
    , app : Maybe App
    , error : Maybe String
    , loadingJob : Bool
    , history : RemoteData Http.Error (List Agave.JobHistory)
    , results : RemoteData Http.Error (List (String, String))
    , startTime : Int -- milliseconds
    , lastPollTime : Int -- milliseconds
    , dialogState : DialogState
    , fileBrowser : Maybe FileBrowser.Model
    }


type DialogState
    = NoDialog
    | CancelDialog (Maybe String)


init : Session -> String -> ( Model, Cmd Msg )
init session id =
    let
        token =
            Session.token session

        loadJobFromAgave =
            Agave.getJob token id |> Http.toTask |> Task.map .result

        loadJobFromPlanB =
            PlanB.getJob token id |> Http.toTask |> Task.map .result

        loadJob = --TODO add more abstraction/types for provider in dedicated module
            if isPlanB id then
                loadJobFromPlanB
            else
                loadJobFromAgave

        loadApp app_name =
            App.fetchByName app_name |> Http.toTask
    in
    ( { session = session
      , jobId = id
      , job = Nothing
      , app = Nothing
      , error = Nothing
      , loadingJob = False
      , history = NotAsked
      , results = NotAsked
      , startTime = 0
      , lastPollTime = 0
      , dialogState = NoDialog
      , fileBrowser = Nothing
      }
      , loadJob
            |> Task.andThen
                (\job ->
                    loadApp job.app_id
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 500 (\time -> FileBrowserMsg (FileBrowser.SearchUsers time)) -- milliseconds
        , Time.every (10*1000) PollJob -- milliseconds
        ]


--TODO change to accept record instead of list of tuples
port createSimPlot : (String, List (String, String, String)) -> Cmd msg



-- UPDATE --


type Msg
    = GetJobCompleted (Result Http.Error (Job, App))
    | GetHistory
    | SetHistory (Result Http.Error (List Agave.JobHistory))
    | ShowOutputs
    | GetResults
    | SetResults (Result Http.Error (List (String, String)))
    | SetJob Agave.Job
    | PollJob Time.Posix
    | CancelJob
    | CancelJobCompleted (Result Http.Error Agave.Job)
    | CloseDialog
    | FileBrowserMsg FileBrowser.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        token =
            Session.token model.session

        loadJobFromAgave =
            Agave.getJob token model.jobId |> Http.toTask |> Task.map .result

        loadJobFromPlanB =
            PlanB.getJob token model.jobId |> Http.toTask |> Task.map .result

        loadJob =
            if isPlanB model.jobId then
                loadJobFromPlanB
            else
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

        GetJobCompleted (Err error) ->
            ( { model | error = Just (Error.toString error) }, Error.redirectLoadError error (Session.navKey model.session) )

        GetHistory ->
            let
                loadHistoryFromAgave =
                    Agave.getJobHistory token model.jobId |> Http.toTask |> Task.map .result

                loadHistoryFromPlanB =
                    PlanB.getJobHistory token model.jobId |> Http.toTask |> Task.map .result

                loadHistory =
                    if isPlanB model.jobId then
                        loadHistoryFromPlanB
                    else
                       loadHistoryFromAgave
            in
            ( { model | history = Loading }, Task.attempt SetHistory loadHistory )

        SetHistory result ->
            ( { model | history = RemoteData.fromResult result }, Cmd.none )

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

        GetResults ->
            let
                owner =
                    model.job |> Maybe.map .owner |> Maybe.withDefault ""

                loadOutputs path =
                    Agave.getJobOutputs owner token model.jobId (Just path)
                        |> Http.toTask
                        |> Task.map .result
                        |> Task.map (List.filter (\r -> r.name /= "." && String.endsWith ".tab" r.name) >> List.map .path) -- filter out current path "." #FIXME hardcoded for .tab files (for ohana-blast) 

                -- Expects relative path
                loadOutput path =
                    Agave.getJobOutput owner token model.jobId path
                        |> Http.toTask |> Task.map (\data -> List.singleton (path, data))

                -- Expects full path
                loadFile path =
                    Agave.getFile token path
                        |> Http.toTask |> Task.map (\data -> List.singleton (path, data))

                -- Get a file or every file in a directory if path ends in "/"
                loadResultData path =
                    if String.endsWith "/" path then
                        -- Get contents of every file in the path
                        loadOutputs path
                            |> Task.andThen
                                (\outputs -> outputs |> List.map loadFile |> Task.sequence |> Task.map List.concat)
                    else
                        loadOutput path

                loadResults =
                    case model.app of
                        Just app ->
                            app.app_results |> List.map (loadResultData << .path) |> Task.sequence |> Task.map List.concat

                        Nothing ->
                            Task.succeed []
            in
            ( { model | results = Loading }
            , Task.attempt SetResults loadResults
            )

        SetResults (Ok results) ->
            let
                cmd =
                    if results == [] then -- File not found
                        Cmd.none
                    else
                        case model.app of
                            Just app ->
                                let
                                    datasets =
                                        List.Extra.lift2 (\a b -> (a.data_type, Tuple.first b, Tuple.second b)) app.app_results results
                                        --TODO change createSimPlot port to accept record instead of list of tuples
                                in
                                createSimPlot ("sim-plot", datasets)

                            Nothing ->
                                Cmd.none
            in
            ( { model | results = Success results }, cmd )

        SetResults (Err error) ->
            ( { model | results = Failure error }, Cmd.none )

        SetJob job ->
            ( { model | job = Just job, loadingJob = False }, Cmd.none )

        PollJob time ->
            case model.job of
                Just job ->
                    if model.loadingJob == False && isRunning job then
                        let
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

                                    Err _ -> -- Ignore polling failure and try next time
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
                        if doPoll then
                            ( { newModel | loadingJob = True }, Task.attempt handleJob loadJob )
                        else
                            ( newModel, Cmd.none )
                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        CancelJob ->
            let
                stopJob =
                    Agave.stopJob token model.jobId
                        |> Http.toTask
                        |> Task.andThen (\_ -> loadJob)
            in
            ( { model | dialogState = CancelDialog Nothing }
            , Task.attempt CancelJobCompleted stopJob
            )

        CancelJobCompleted (Ok job) ->
            let
                errorMsg =
                    "A cancellation request was sent.  This may or may not result in the termination of the job depending on its state."
            in
            ( { model | dialogState = CancelDialog (Just errorMsg), job = Just job }, Cmd.none )

        CancelJobCompleted (Err error) ->
            ( { model | dialogState = CancelDialog (Just <| Error.toString error) }, Cmd.none )

        CloseDialog ->
            ( { model | dialogState = NoDialog }, Cmd.none )

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
                username =
                    Session.getUser model.session |> Maybe.map .user_name |> Maybe.withDefault ""

                deUrl =
                    "https://de.cyverse.org/de/?type=data&folder=/iplant/home/" ++ username ++ "/archive/jobs/job-" ++ model.jobId --FIXME move base url to config
            in
            div [ class "container" ]
                [ div [ class "pb-2 mt-5 mb-2 border-bottom", style "width" "100%" ]
                    [ h1 [ class "font-weight-bold d-inline" ]
                        [ span [ style "color" "dimgray" ] [ text "Job" ]
                        , small [ class "ml-3", style "color" "gray" ] [ text job.name ]
                        ]
                    , span [ class "float-right text-secondary", style "font-size" "2em" ]
                        [ text "Status: ", viewJobStatus job.status ]
                    ]
                , viewJob job app
                , br [] []
                , Page.viewTitle2 "Inputs" False
                , viewInputs job.inputs
                , br [] []
                , Page.viewTitle2 "Parameters" False
                , viewParameters job.parameters
                , br [] []
                , if not (isPlanB job.id) then
                    div []
                        [ Page.viewTitle2 "Settings" False
                        , viewSettings job
                        , br [] []
                        ]
                  else
                    text ""
                , Page.viewTitle2 "History" False
                , viewHistory model.history
                , br [] []
                , div [ class "mt-4 border-bottom", style "width" "100%" ]
                    [ h2 [ class "font-weight-bold d-inline" ]
                        [ span [ style "color" "dimgray" ] [ text "Outputs" ]
                        ]
                    , span [ class "float-right pt-2" ]
                        [ text "View output files in the "
                        , a [ target "_blank", href deUrl ]
                            [ text "CyVerse Data Store "
                            , span [ class "align-baseline ml-2"] [ Icon.externalLink ]
                            ]
                        ]
                    ]
                , viewOutputs model
                , br [] []
                , br [] []
                , Page.viewTitle2 "Results" False
                , viewResults model
                , case model.dialogState of
                    CancelDialog msg ->
                        viewCancelDialog msg

                    _ ->
                        Page.viewBlank
              ]

        ( _, _ ) ->
            case model.error of
                Nothing ->
                    Page.viewSpinnerCentered

                Just error ->
                    div [ class "container" ]
                        [ div [ class "alert alert-danger m-5 p-5" ] [ text error ] ]


viewJob : Job -> App -> Html Msg
viewJob job app =
    div []
        [ table [ class "table table-borderless table-sm" ]
            [ tr []
                [ th [ class "w-25" ] [ text "ID" ]
                , td [] [ text job.id ]
                ]
            , tr []
                [ th [] [ text "Name" ]
                , td [] [ text job.name ]
                ]
            , tr []
                [ th [] [ text "App" ]
                , td [] [ a [ Route.href (Route.App (String.fromInt app.id)) ] [ text app.name ] ]
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
                    [ viewProgress job.status
                    , if isRunning job then
                        button [ class "btn btn-outline-secondary btn-sm ml-2 align-top", onClick CancelJob ] [ text "Cancel" ]
                     else
                        text ""
                    ]
                , td [] []
                ]
            ]
            , if isFailed job && job.lastStatusMessage /= "" then
                div [ class "alert alert-danger" ] [ text job.lastStatusMessage ]
              else
                text ""
        ]


viewProgress : String -> Html msg
viewProgress status =
    let
        progressBar pct =
            let
                label =
                    String.replace "_" " " status
            in
            div [ class "progress float-left d-inline-block", style "width" "20em", style "height" "2.5em" ]
                [ div [ class "progress-bar progress-bar-striped progress-bar-animated", style "width" ((String.fromInt pct) ++ "%"), style "height" "2.5em",
                        attribute "role" "progressbar", attribute "aria-valuenow" (String.fromInt pct), attribute "aria-valuemin" "0", attribute "aria-valuemax" "100" ]
                    [ text label ]
                ]
    in
    case String.toUpper status of
        "CREATED" -> progressBar 10
        "PENDING" -> progressBar 20
        "PROCESSING_INPUTS" -> progressBar 30
        "STAGING_INPUTS" -> progressBar 40
        "STAGED" -> progressBar 45
        "SUBMITTING" -> progressBar 50
        "STAGING_JOB" -> progressBar 55
        "QUEUED" -> progressBar 60
        "BLOCKED" -> progressBar 60
        "RUNNING" -> progressBar 70
        "CLEANING_UP" -> progressBar 80
        "ARCHIVING" -> progressBar 90
        "ARCHIVING_FINISHED" -> progressBar 95
        _ -> viewJobStatus status


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


viewSettings : Agave.Job -> Html Msg
viewSettings job =
    table [ class "table" ]
        [ tbody []
            [ tr []
                [ th [ class "w-25" ] [ text "Queue" ]
                , td [] [ text job.remoteQueue ]
                ]
            , tr []
                [ th [ class "w-25" ] [ text "Time limit" ]
                , td [] [ text (String.fromInt job.maxHours) ]
                ]
            ]
        ]


viewHistory : RemoteData Http.Error (List Agave.JobHistory) -> Html Msg
viewHistory history =
    div [ class "border-top w-100 pt-2" ]
        [ case history of
            Success h ->
                if h == [] then
                    text "None"
                else
                    div [] (List.map viewEvent h)

            Loading ->
                Page.viewSpinner

            Failure error ->
                Error.view error False

            NotAsked ->
                button [ class "btn btn-outline-secondary", onClick GetHistory ] [ text "Show History" ]
        ]


viewEvent : Agave.JobHistory -> Html Msg
viewEvent event =
    div [ class "row" ]
        [ div [ class "col text-nowrap" ] [ text event.created ]
        , div [ class "col text-nowrap" ] [ text event.status ]
        , div [ class "col-6" ] [ text event.description ]
        ]


viewOutputs : Model -> Html Msg
viewOutputs model =
    div []
        [ case model.job of
            Just job ->
                case job.status of
                    "FINISHED" ->
                        case model.fileBrowser of
                            Nothing ->
                                button [ class "btn btn-outline-secondary mt-2", onClick ShowOutputs ] [ text "Show Outputs" ]

                            Just fileBrowser ->
                                div [ class "row", style "margin-left" "0.1em" ]
                                    [ div [ style "width" "70%", style "min-height" "20em" ]
                                        [ FileBrowser.view fileBrowser |> Html.map FileBrowserMsg ]
                                    , div [ class "ml-4 pt-2", style "width" "25%" ]
                                        [ case FileBrowser.getSelected fileBrowser of
                                            [] ->
                                                p []
                                                    [ br [] []
                                                    , text "Here are the output files from the job."
                                                    , br [] []
                                                    , br [] []
                                                    , text "Click to select a file or directory."
                                                    , br [] []
                                                    , text "Double-click to open a file or directory."
                                                    ]

                                            file :: _ ->
                                                if file.name == ".. (previous)" then -- FIXME
                                                    text ""
                                                else
                                                    viewFileInfo (Session.token model.session) file
                                        ]
                                    ]

                    "FAILED" ->
                        text "None"

                    _ ->
                        text "Job is not FINISHED, please wait ..."

            Nothing ->
                text ""
        ]


--TODO move into FileBrowser.elm
viewFileInfo : String -> FileResult -> Html Msg
viewFileInfo token file =
    let
        myLocale =
            { usLocale | decimals = 0 }

        deleteText =
            "Are you sure you want to remove the " ++
                (if file.type_ == "dir" then
                    "directory"
                 else
                    file.type_
                ) ++
                " '" ++ file.name ++ "'?"

        deleteMsg =
            FileBrowserMsg (FileBrowser.OpenDialog (FileBrowser.ConfirmationDialog deleteText (FileBrowser.DeletePath file.path)))

        deUrl =
            "https://de.cyverse.org/de/?type=data&folder=/iplant/home" ++ --TODO move base url to config file
                (if file.type_ == "dir" then
                     file.path
                else
                    dropFileName file.path
                )

        dropFileName s =
            String.split "/" s |> List.reverse |> List.drop 1 |> List.reverse |> String.join "/" -- pretty inefficient
    in
    div []
        [ table [ class "table table-borderless table-sm" ]
            [ tbody []
                [ tr []
                    [ th [] [ text "Name " ]
                    , td [] [ text file.name ]
                    ]
                , tr []
                    [ th [] [ text "Type " ]
                    , td [] [ text file.type_ ]
                    ]
                , tr []
                    [ th [] [ text "Size " ]
                    , td [] [ text ((file.length |> toFloat |> format myLocale) ++ " bytes") ]
                    ]
                , tr []
                    [ th [] [ text "Last modified " ]
                    , td [] [ text file.lastModified ]
                    ]
--                , tr []
--                    [ td [] [ button [ class "btn btn-link btn-xs" ]
--                        [ span [ class "glyphicon glyphicon-plus" ] [], text " Add to Sample" ] ]
--                    ]
                ]
            ]
        , if file.type_ == "file" then
            a [ class "mt-2", href (apiBaseUrl ++ "/download" ++ file.path ++ "?token=" ++ token) ]
                [ Icon.cloudDownload, text " Download" ]
          else
            text ""
        , a [ class "mt-2 d-block", href "", onClick (FileBrowserMsg (FileBrowser.OpenShareDialog file.path)) ]
            [ Icon.user, text " Share" ]
        , a [ class "mt-2 d-block", href deUrl, target "_blank" ]
            [ Icon.externalLinkSquare, text " View in CyVerse DE" ]
        --, a [ class "mt-2 d-block", href "", onClick deleteMsg ]
        --    [ Icon.trash, text " Delete" ]
        ]


viewResults : Model -> Html Msg
viewResults model =
    let
        body =
            case model.job of
                Just job ->
                    case job.status of
                        "FINISHED" ->
                            case model.results of
                                Success results ->
                                    if results == [] then
                                        text "None"
                                    else
                                        Page.viewBlank

                                Loading ->
                                    Page.viewSpinner

                                Failure error ->
                                    Error.view error False

                                NotAsked ->
                                    button [ class "btn btn-outline-secondary", onClick GetResults ] [ text "Show Results" ]

                        "FAILED" ->
                            text "None"

                        _ ->
                            div [ class "italic" ] [ text "Job is not FINISHED, please wait ..." ]

                Nothing ->
                    Page.viewBlank
    in
    div [ class "border-top w-100 pt-2" ]
        [ body
        , div [ id "sim-plot" ] [] -- must always be present for accessibility from heatmap.js
        ]


viewCancelDialog : Maybe String -> Html Msg
viewCancelDialog maybeMessage =
    let
        (body, footer) =
            case maybeMessage of
                Nothing ->
                    ( Page.viewSpinner, div [] [ text " " ] )

                Just message ->
                    ( div [ class "alert alert-info m-3" ]
                        [ p [] [ text message ] ]
                    , button [ class "btn btn-outline-secondary", onClick CloseDialog ] [ text "OK" ]
                    )
    in
    Page.viewDialog "Submitting Job"
        [ body ]
        [ footer ]
        CloseDialog


isPlanB : String -> Bool
isPlanB id =
    String.startsWith "planb" id


isRunning : Agave.Job -> Bool
isRunning job =
    job.status /= "FINISHED" && job.status /= "FAILED" && job.status /= "STOPPED"


isFailed : Agave.Job -> Bool
isFailed job =
    job.status == "FAILED" || job.status == "STOPPED"