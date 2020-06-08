module Page.Analyze exposing (Model, Msg, init, toSession, subscriptions, update, view)

import Session exposing (Session(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Page
import Route exposing (Route)
import Error
import Http
import RemoteData exposing (RemoteData(..))
import Json.Decode as Decode exposing (Decoder)
import Task exposing (Task)
import Icon
import App exposing (App)
import Agave exposing (Job, FileResult)
import PlanB
import FileBrowser
import Config exposing (apiBaseUrl)



---- MODEL ----


type alias Model =
    { session : Session
    , apps : RemoteData Http.Error (List App)
    , jobs : RemoteData Http.Error (List Job)
    , tab : String
    , query : String
    , fileBrowser : FileBrowser.Model
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
      , fileBrowser = FileBrowser.init session Nothing
      }
      , Cmd.batch
          [ App.fetchAll |> Http.send GetAppsCompleted
          , getJobs
          ]
    )


toSession : Model -> Session
toSession model =
    model.session


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Sub.map UploadFileBegin (FileBrowser.fileUploadFileSelected (Decode.decodeString FileBrowser.fileDecoder >> Result.toMaybe))
        , Sub.map UploadFileEnd (FileBrowser.fileUploadDone (Decode.decodeString (Agave.responseDecoder Agave.decoderUploadResult) >> Result.toMaybe))
        ]



-- UPDATE --


type Msg
    = GetAppsCompleted (Result Http.Error (List App))
    | GetJobsCompleted (Result Http.Error (List Job, List Job))
    | SetTab String
    | SetQuery String
    | FileBrowserMsg FileBrowser.Msg
    | UploadFileBegin (Maybe FileBrowser.FileToUpload)
    | UploadFileEnd (Maybe (Agave.Response Agave.UploadResult))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetAppsCompleted result ->
            ( { model | apps = RemoteData.fromResult result }, Cmd.none )

        GetJobsCompleted (Ok (agaveJobs, planbJobs)) ->
            ( { model | jobs = Success (agaveJobs ++ planbJobs) }, Cmd.none )

        GetJobsCompleted (Err error) ->
            ( { model | jobs = NotAsked }, Cmd.none ) -- Show login message

        SetTab label ->
            let
                (subModel, subCmd) =
                    case label of
                        "Data" ->
                            case model.session of
                                LoggedIn _ _ _ _ ->
                                    FileBrowser.update model.session FileBrowser.RefreshPath model.fileBrowser

                                _ ->
                                    ( model.fileBrowser, Cmd.none )

                        _ ->
                            ( model.fileBrowser, Cmd.none )
            in
            ( { model | tab = label, fileBrowser = subModel }
            , Cmd.map FileBrowserMsg subCmd
            )

        SetQuery query ->
            ( { model | query = query }, Cmd.none )

        FileBrowserMsg subMsg ->
            let
                ( newFileBrowser, subCmd ) =
                    FileBrowser.update model.session subMsg model.fileBrowser
            in
            ( { model | fileBrowser = newFileBrowser }, Cmd.map FileBrowserMsg subCmd )

        UploadFileBegin _ ->
            let
                (subModel, subCmd) =
                    FileBrowser.update model.session (FileBrowser.OpenDialog (FileBrowser.MessageDialog "Uploading...")) model.fileBrowser
            in
            ( { model | fileBrowser = subModel }, Cmd.map FileBrowserMsg subCmd )

        UploadFileEnd _ ->
            let
                (subModel, subCmd) =
                    FileBrowser.update model.session FileBrowser.RefreshPath model.fileBrowser
            in
            ( { model | fileBrowser = subModel }, Cmd.map FileBrowserMsg subCmd )



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
                    , div [ class "mt-4 ml-3 alert alert-secondary" ]
                        [ a [ Route.href Route.Login ] [ text "Sign-in" ]
                        , text " to see your jobs"
                        ]
                    )

        dataRow =
            case model.session of
                LoggedIn _ _ _ cred ->
                    viewData cred.token model.fileBrowser

                _ ->
                    div [ class "mt-4 ml-3 alert alert-secondary" ]
                        [ a [ Route.href Route.Login ] [ text "Sign-in" ]
                        , text " to see your data"
                        ]

        navItem label count =
            li [ class "nav-item text-nowrap mr-5 font-weight-bold" ]
                [ a [ class "nav-link text-secondary", classList [ ("border rounded", model.tab == label) ], href "", onClick (SetTab label) ]
                    [ text label ]
                ]
    in
    div [ class "container" ]
        [ ul [ class "nav nav-pills mt-5 h5" ]
            [ navItem "Apps" numApps
            , navItem "Jobs" numJobs
            , navItem "Data" 0
            ]
        , if model.tab == "Jobs" then
            jobsRow
          else if model.tab == "Data" then
            dataRow
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
    table [ class "table mt-4" ]
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
    div []
        [ input [ class "float-right w-25 mb-2", placeholder "Search", onInput SetQuery ] []
        , table [ class "table" ]
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
        ]


viewData : String -> FileBrowser.Model -> Html Msg
viewData token fileBrowser =
    div [ class "row ml-3 mt-4" ]
        [ div [ style "width" "70%", style "min-height" "20em" ]
            [ FileBrowser.view fileBrowser |> Html.map FileBrowserMsg ]
        , div [ class "ml-4", style "width" "25%" ]
            [ case FileBrowser.getSelected fileBrowser of
                [] ->
                    div [ class "font-weight-light text-secondary" ]
                        [ text "Here are the contents of your CyVerse Data Store home directory."
                        , br [] []
                        , br [] []
                        , text "Click to select a file or directory."
                        , br [] []
                        , text "Double-click to open a file or directory."
                        ]

                file :: _ ->
                    if file.name == ".. (previous)" then
                        text ""
                    else
                        viewFileInfo token file
            ]
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
        , a [ class "mt-2 d-block", href "", onClick deleteMsg ]
            [ Icon.trash, text " Delete" ]
        ]