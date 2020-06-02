port module FileBrowser exposing (..)

{-| A heavy-weight file browser component

"Heavy-weight" means that it has its own internal state and update routine, a practice which is discouraged in Elm.
However there is so much functionality packed into this module that it seems justified in this case.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onDoubleClick, onInput)
--import Events exposing (onKeyDown)
import Task exposing (Task)
import Time
import Http
import Route
import Filesize
import Page exposing (viewSpinner, viewDialog)
import SearchableDropdown
import Session exposing (Session)
import Agave exposing (FileResult, PermissionResult, Permission)
import List.Extra
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (required)
import Icon
import Error



-- MODEL


type Model
    = Model InternalModel


type alias InternalModel =
    { currentUserName : String
    , path : String
    , rootPath : String
    , homePath : String
    , sharedPath : String
    , selectedPaths : Maybe (List String)
    , pathFilter : String
    , contents : List FileResult
    , isBusy : Bool
    , config : Config
    , newFolderName : String
    , filePath : Maybe String
    , fileContent : Maybe String
    , fileErrorMessage : Maybe String
    , doUserSearch : Bool
    , searchStartTime : Int -- milliseconds
    , filePermissions : Maybe (List PermissionResult)
    , shareDropdownState : SearchableDropdown.State
    , errorMessage : Maybe String
    , dialogState : DialogState
    }


type alias Config =
    { showMenuBar : Bool
    , showNewFolderButton : Bool
    , showUploadFileButton : Bool
    , allowDirSelection : Bool
    , allowMultiSelection : Bool
    , allowFileViewing : Bool
    , homePath : Maybe String
    }


defaultConfig : Config
defaultConfig =
    { showMenuBar = True
    , showNewFolderButton = True
    , showUploadFileButton = True
    , allowDirSelection = True
    , allowMultiSelection = False
    , allowFileViewing = True
    , homePath = Nothing
    }


type DialogState
    = NoDialog
    | ConfirmationDialog String Msg
    | MessageDialog String
    | NewFolderDialog
    | ShareDialog
    | ViewFileDialog


init : Session -> Maybe Config -> Model --Task Http.Error Model
init session maybeConfig =
    let
        username =
            Session.getUser session |> Maybe.map .user_name |> Maybe.withDefault ""

        config =
            maybeConfig |> Maybe.withDefault defaultConfig

        startingPath =
            config.homePath |> Maybe.withDefault ("/" ++ username)
    in
    Model
        { currentUserName = username
        , path = startingPath
        , rootPath = startingPath
        , homePath = startingPath
        , sharedPath = "/shared"
        , selectedPaths = Nothing
        , pathFilter = "Home"
        , contents = []
        , isBusy = True
        , config = config
        , newFolderName = ""
        , filePath = Nothing
        , fileContent = Nothing
        , fileErrorMessage = Nothing
        , doUserSearch = False
        , searchStartTime = 0
        , filePermissions = Nothing
        , shareDropdownState = SearchableDropdown.init
        , errorMessage = Nothing
        , dialogState = NoDialog
        }


loadPath : String -> String -> Task Http.Error (List FileResult)
loadPath token path =
    Agave.getFileList token path |> Http.toTask |> Task.map .result


maxViewFileSz = 5000



-- UPDATE


type Msg
    = SetFilter String
    | SetPath String
    | KeyDown Int
    | SelectPath String
    | RefreshPath
    | LoadPath String
    | LoadPathCompleted (Result Http.Error (List FileResult))
    | OpenPath String Int
    | OpenPathCompleted (Result Http.Error String)
    | SetNewFolderName String
    | CreateNewFolderCompleted (Result Http.Error (Agave.EmptyResponse))
    | CreateNewFolder
    | DeletePath String
    | DeletePathCompleted (Result Http.Error (Agave.EmptyResponse))
    | GetPermissionCompleted (Result Http.Error (List PermissionResult))
    | SetShareUserName String
    | SetSearchStartTime Time.Posix
    | SearchUsers Time.Posix
    | SearchUsersCompleted (Result Http.Error (List Agave.Profile))
    | ShareWithUser String String String
    | ShareWithUserCompleted (Result Http.Error (Agave.EmptyResponse))
    | OpenShareDialog String
    | OpenDialog DialogState
    | CloseDialog
    | UploadFile


update : Session -> Msg -> Model -> (Model, Cmd Msg)
update session msg (Model internalModel) =
    updateInternal session msg internalModel
        |> Tuple.mapFirst Model


updateInternal : Session -> Msg -> InternalModel -> ( InternalModel, Cmd Msg )
updateInternal session msg model =
    let
        token =
            Session.token session
    in
    case msg of
        SetFilter value ->
            let
                newPath =
                    if value == "Shared" then
                        model.sharedPath
                    else -- "Home"
                        model.homePath
            in
            updateInternal session (LoadPath newPath) { model | path = newPath, pathFilter = value }

        SetPath path ->
            ( { model | path = path }, Cmd.none )

        KeyDown key ->
            if key == 13 then -- enter key
                updateInternal session (LoadPath model.path) model
            else
                ( model, Cmd.none )

        SelectPath path ->
            let
                newPaths =
                    case model.selectedPaths of
                        Nothing ->
                            Just (List.singleton path)

                        Just paths ->
                            if List.member path paths then
                                Just (List.filter (\p -> p /= path) paths) -- unselect
                            else
                                if model.config.allowMultiSelection then
                                    Just (path :: paths)
                                else
                                    Just (List.singleton path)
            in
            ( { model | selectedPaths = newPaths }, Cmd.none )

        RefreshPath ->
            updateInternal session (LoadPath model.path) { model | dialogState = NoDialog }

        LoadPath path ->
            ( { model | path = path, selectedPaths = Nothing, errorMessage = Nothing, isBusy = True }
            , Task.attempt LoadPathCompleted (loadPath token path)
            )

        LoadPathCompleted (Ok files) ->
            let
                -- Manufacture a previous path
                previous =
                    { name = ".. (previous)"
                    , path = determinePreviousPath model.path
                    , type_ = "dir"
                    , format = ""
                    , length = 0
                    , lastModified = ""
                    , mimeType = ""
                    }

                -- Remove current path
                filtered =
                    List.filter (\f -> f.name /= ".") files

                newFiles =
                    -- Only show previous path if not at top-level
                    if model.path /= model.rootPath && model.path /= model.sharedPath then
                        previous :: filtered
                    else
                        filtered
            in
            ( { model | contents = newFiles, isBusy = False }, Cmd.none )

        LoadPathCompleted (Err error) ->
            let
                (errMsg, cmd) =
                    case error of
                        Http.NetworkError ->
                            ("Cannot connect to remote host", Cmd.none)

                        Http.BadStatus response ->
                            case response.status.code of
                                401 ->
                                    ("Unauthorized", Route.replaceUrl (Session.navKey session) Route.Login) -- redirect to Login page

                                403 ->
                                    ("Permission denied", Cmd.none)

                                _ ->
                                    case String.length response.body of
                                        0 ->
                                            ("Bad status", Cmd.none)

                                        _ ->
                                            (response.body, Cmd.none)

                        _ ->
                            (Error.toString error, Cmd.none)
            in
            ( { model | errorMessage = (Just errMsg), isBusy = False }, cmd )

        OpenPath path length ->
            let
                chunkSz =
                    Basics.min (length-1) (maxViewFileSz-1)

                openPath =
                    Agave.getFileRange token path (Just (0, chunkSz)) |> Http.toTask
            in
            ( { model | dialogState = ViewFileDialog, filePath = Just path }
            , Task.attempt OpenPathCompleted openPath
            )

        OpenPathCompleted (Ok data) ->
            ( { model | fileContent = Just data }, Cmd.none )

        OpenPathCompleted (Err error) ->
            ( { model | fileErrorMessage = (Just (Error.toString error)) }, Cmd.none )

        SetNewFolderName name ->
            ( { model | newFolderName = name }, Cmd.none )

        CreateNewFolder ->
            let
                createFolder =
                    Agave.mkdir token model.path model.newFolderName |> Http.toTask
            in
            ( model, Task.attempt CreateNewFolderCompleted createFolder )

        CreateNewFolderCompleted (Ok _) ->
            updateInternal session RefreshPath { model | dialogState = NoDialog }

        CreateNewFolderCompleted (Err error) ->
            ( { model | dialogState = NoDialog, errorMessage = Just (Error.toString error) }, Cmd.none )

        DeletePath path ->
            if path == "" || path == "/" || path == model.homePath then -- don't let them try something stupid
                ( { model | dialogState = NoDialog }, Cmd.none )
            else
                let
                    delete =
                        Agave.delete token path |> Http.toTask
                in
                ( { model | isBusy = True, dialogState = NoDialog }, Task.attempt DeletePathCompleted delete )

        DeletePathCompleted (Ok _) ->
            updateInternal session RefreshPath model

        DeletePathCompleted (Err error) ->
            ( { model | isBusy = False }, Cmd.none )

        OpenShareDialog path ->
            let
                getPermission =
                    Agave.getFilePermission token path |> Http.toTask |> Task.map .result
            in
            ( { model | dialogState = ShareDialog }, Task.attempt GetPermissionCompleted getPermission )

        GetPermissionCompleted (Ok permissions) ->
            let
                notAllowed = --FIXME move to config file
                    [ "dooley", "vaughn", "rodsadmin", "jstubbs", "jfonner", "eriksf", "QuickShare"
                    , "admin2", "admin_proxy", "agave", "bisque-adm", "de-irods", "has_admin", "ibp-proxy"
                    , "ipc_admin", "ipcservices", "proxy-de-tools", "uk_admin", "uportal_admin2"
                    ]

                filtered =
                    List.filter (\p -> List.member p.username notAllowed |> not) permissions
            in
            ( { model | filePermissions = Just filtered }, Cmd.none )

        GetPermissionCompleted (Err error) -> -- TODO
            ( { model | filePermissions = Nothing }, Cmd.none )

        SetShareUserName name ->
            let
                dropdownState =
                    model.shareDropdownState
            in
            if String.length name >= 3 then
                ( { model | shareDropdownState = { dropdownState | value = name }, doUserSearch = True }
                , Task.perform SetSearchStartTime Time.now
                )
            else
                ( { model | shareDropdownState = { dropdownState | value = name, results = [] }, doUserSearch = False }
                , Cmd.none
                )

        SetSearchStartTime time ->
            ( { model | searchStartTime = Time.posixToMillis time }, Cmd.none )

        SearchUsers time ->
            if model.doUserSearch && Time.posixToMillis time - model.searchStartTime >= 500 then
                let
                    searchProfiles =
                        Agave.searchProfiles token model.shareDropdownState.value |> Http.toTask |> Task.map .result
                in
                ( { model | doUserSearch = False }
                , Task.attempt SearchUsersCompleted searchProfiles
                )
            else
                ( model, Cmd.none )

        SearchUsersCompleted (Ok users) ->
            let
                userDisplayName user =
                    user.first_name ++ " " ++ user.last_name ++ " (" ++ user.username ++ ")"

                results =
                    List.map (\u -> (u.username, userDisplayName u)) users

                dropdownState =
                    model.shareDropdownState
            in
            ( { model | shareDropdownState = { dropdownState | results = results } }, Cmd.none )

        SearchUsersCompleted (Err error) -> -- TODO
            ( model, Cmd.none )

        ShareWithUser permission username _ ->
            let
                dropdownState =
                    model.shareDropdownState

                newModel =
                    { model | shareDropdownState = { dropdownState | value = "", results = [] } }

                firstSelected =
                    model.selectedPaths |> Maybe.withDefault [] |> List.head |> Maybe.withDefault ""
            in
            let
                noChange =
                    case model.filePermissions of
                        Nothing ->
                            False

                        Just permissions ->
                            List.any (\p -> p.username == username && (permissionDesc p.permission) == permission) permissions

                agavePerm =
                    case permission of
                         "read/write" ->
                             "READ_WRITE"

                         "none" ->
                              "NONE"

                         _ ->
                             "READ"

                shareFile =
                    Agave.setFilePermission token username agavePerm firstSelected |> Http.toTask
            in
            if noChange then
                ( model, Cmd.none )
            else
                ( newModel, Task.attempt ShareWithUserCompleted shareFile )

        ShareWithUserCompleted (Ok _) ->
            let
                firstSelected =
                    model.selectedPaths |> Maybe.withDefault [] |> List.head |> Maybe.withDefault ""
            in
            updateInternal session (OpenShareDialog firstSelected) model

        ShareWithUserCompleted (Err error) -> -- TODO
            ( model, Cmd.none )

        OpenDialog dialogState ->
            ( { model | dialogState = dialogState }, Cmd.none )

        CloseDialog ->
            ( { model | dialogState = NoDialog }, Cmd.none )

        UploadFile ->
            ( model, fileUploadOpenBrowser (token, model.path) )


determinePreviousPath : String -> String
determinePreviousPath path =
    let
        l =
            String.split "/" path
        n =
            List.length l
    in
    List.take (n-1) l |> String.join "/"



-- VIEW


view : Model -> Html Msg
view (Model {currentUserName, path, pathFilter, contents, selectedPaths, isBusy, errorMessage, dialogState,
            filePath, fileContent, fileErrorMessage, filePermissions, shareDropdownState, config
            }) =
    let
        menuBar =
            div [ class "d-inline" ]
                [ div [ class "input-group" ]
                    [ div [ class "input-group-prepend" ]
                        [ filterButton "Home"
                        , filterButton "Shared"
                        ]
                    , input [ class "form-control",  type_ "text", size 30, value path, placeholder "Enter path", onInput SetPath ] [] -- onKeyDown KeyDown ] []
                    , div [ class "input-group-append" ]
                        [ button [ class "btn btn-outline-secondary", type_ "button", onClick (LoadPath path) ] [ text "Go " ]
                        ]
                    ]
                , if config.showNewFolderButton then
                    button [ class "btn btn-outline-secondary btn-sm margin-right", type_ "button", onClick (OpenDialog NewFolderDialog) ]
                        [ Icon.folder, text " New Folder" ]
                  else
                    text ""
                , if config.showUploadFileButton then
                    button [ class "btn btn-outline-secondary btn-sm", type_ "button", onClick UploadFile ]
                        [ Icon.upload, text " Upload File" ]
                  else
                    text ""
                ]

        filterButton label =
            let
                isActive =
                    label == pathFilter
            in
            button [ class "btn btn-outline-secondary", classList [("active", isActive)], type_ "button", onClick (SetFilter label) ]
                [ text label ]
    in
    div []
        [ if config.showMenuBar then
            menuBar
          else
            text ""
        , if errorMessage /= Nothing then
            div [ class "alert alert-danger" ] [ text (Maybe.withDefault "An error occurred" errorMessage) ]
          else if isBusy then
            viewSpinner
          else
            div [ style "overflow-y" "auto", style "height" "100%" ]
                [ viewFileTable config contents selectedPaths ]
        , input [ type_ "file", id "fileToUpload", name "fileToUpload", style "display" "none" ] [] -- hidden input for file upload plugin, "fileToUpload" name is required by Agave
        , case dialogState of
            NoDialog ->
                text ""

            ConfirmationDialog message yesMsg ->
                Page.viewConfirmationDialog message CloseDialog yesMsg

            MessageDialog message ->
                Page.viewProgressDialog message CloseDialog

            NewFolderDialog ->
                viewNewFolderDialog False

            ShareDialog ->
                viewShareDialog (filePermissions |> Maybe.withDefault []) currentUserName shareDropdownState False fileErrorMessage

            ViewFileDialog ->
                viewFileDialog (filePath |> Maybe.withDefault "") (fileContent |> Maybe.withDefault "") False fileErrorMessage
        ]


viewFileTable : Config -> List FileResult -> Maybe (List String) -> Html Msg
viewFileTable config files selectedPaths =
    let
        fileRow file =
            let
                isSelected =
                    selectedPaths
                        |> Maybe.withDefault []
                        |> (\paths ->
                            List.member file.path paths && (file.type_ == "file" || config.allowDirSelection)
                        )

                action f =
                    if f.name /= ".. (previous)" then --FIXME kludge
                        (onClick (SelectPath file.path) ::
                            (if f.type_ == "dir" then
                                [ onDoubleClick (LoadPath f.path) ]
                              else if f.type_ == "file" && config.allowFileViewing then
                                [ onDoubleClick (OpenPath f.path f.length) ]
                              else
                                []
                            )
                        )
                    else
                        []
            in
            tr ((action file) ++ [ classList [ ("bg-primary", isSelected), ("text-light", isSelected) ] ])
                [ td []
                    [ if file.type_ == "dir" then
                        a [ href "", classList [ ("text-light", isSelected) ], onClick (LoadPath file.path) ] [ text file.name ]
                    else
                        text file.name
                    ]
                , td [ class "text-nowrap" ]
                    [ if file.length > 0 then
                        text (Filesize.format file.length)
                      else
                        text ""
                    ]
                ]
    in
    table [ class "table table-sm" ]
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Size" ]
                ]
            ]
        , tbody [ style "user-select" "none" ]
            (List.map fileRow files)
        ]

--tableConfig : Config -> Maybe (List String) -> Table.Config FileResult Msg
--tableConfig config selectedRowIds =
--    Table.customConfig
--        { toId = .path
--        , toMsg = SetTableState
--        , columns =
--            [ nameColumn
--            , sizeColumn
--            ]
--        , customizations =
--            { defaultCustomizations | tableAttrs = toTableAttrs, rowAttrs = toRowAttrs config selectedRowIds }
--        }
--
--
--nameColumn : Table.Column FileResult Msg
--nameColumn =
--    Table.veryCustomColumn
--        { name = "Name"
--        , viewData = nameLink
--        , sorter =
--            Table.increasingOrDecreasingBy
--                (\data ->
--                    if data.type_ == "dir" then -- sort dirs before files
--                        "..." ++ data.name
--                    else data.name
--                )
--        }
--
--
--nameLink : FileResult -> Table.HtmlDetails Msg
--nameLink file =
--    if file.type_ == "dir" then
--        Table.HtmlDetails []
--            [ a [ onClick (LoadPath file.path) ] [ text file.name ]
--            ]
--    else
--        Table.HtmlDetails [] [ text file.name ]
--
--
--sizeColumn : Table.Column FileResult Msg
--sizeColumn =
--    Table.veryCustomColumn
--        { name = "Size"
--        , viewData = (\file -> Table.HtmlDetails [] [ if (file.length > 0) then (text (Filesize.format file.length)) else text "" ])
--        , sorter = Table.increasingOrDecreasingBy .length
--        }


viewNewFolderDialog : Bool -> Html Msg
viewNewFolderDialog isBusy =
    let
        content =
            if isBusy then
                Page.viewSpinner
            else
                input [ class "form-control", type_ "text", size 20, placeholder "Enter the name of the new folder", onInput SetNewFolderName ] []

        footer =
            [ button [ class "btn btn-outline-secondary float-left", onClick CloseDialog, disabled isBusy ] [ text "Cancel" ]
            , button [ class "btn btn-primary", onClick CreateNewFolder, disabled isBusy ] [ text "OK" ]
            ]
    in
    viewDialog "Create New Folder"
        [ content ]
        footer
        CloseDialog


viewFileDialog : String -> String -> Bool -> Maybe String -> Html Msg
viewFileDialog path data isBusy errorMsg =
    let
        body =
            if errorMsg /= Nothing then
                div [ class "alert alert-danger" ] [ Maybe.withDefault "" errorMsg |> text ]
            else if isBusy then
                viewSpinner
            else
                div []
                    [ span [ class "text-monospace" ] [ text path ]
                    , pre [ class "border p-2", style "overflow" "auto", style "background-color" "#E7E7E7", style "max-height" "50vh" ] [ text data ]
                    ]

        footer =
            div [ class "row text-right", style "width" "100%" ]
                [ em []
                    [ if errorMsg == Nothing && not isBusy then
                        text <| "Showing first " ++ (String.fromInt maxViewFileSz) ++ " bytes only"
                      else
                        text ""
                    ]
                , div [ class "col" ]
                    [ button [ class "btn btn-primary", onClick CloseDialog ] [ text "Close" ] ]
                ]
    in
    viewDialog "View File"
        [ body ]
        [ footer ]
        CloseDialog


viewShareDialog : List PermissionResult -> String -> SearchableDropdown.State -> Bool -> Maybe String -> Html Msg
viewShareDialog permissions currentUserName dropdownState isBusy errorMsg =
    let
        body =
            if errorMsg /= Nothing then
                div [ class "alert alert-danger" ] [ Maybe.withDefault "" errorMsg |> text ]
            else if isBusy then
                viewSpinner
            else
                div []
                    [ text "Who has access"
                    , div [ class "border-top pb-5", style "overflow-y" "auto", style "max-height" "30vh" ]
                        [ viewPermissions currentUserName permissions
                        ]
                    , addUserPanel
                    ]

        addUserPanel =
            div [ class "form-group mt-5" ]
                [ div [] [ text "Add a person:" ]
                , div []
                    [ SearchableDropdown.view shareDropdownConfig dropdownState ]
                ]
    in
    viewDialog "Share Item"
        [ body ]
        []
        CloseDialog


shareDropdownConfig : SearchableDropdown.Config Msg Msg
shareDropdownConfig =
    { placeholder = "Enter the name of the person to add "
    , autofocus = False
    , inputMsg = SetShareUserName
    , selectMsg = ShareWithUser "read-only"
    }


viewPermissions : String -> List PermissionResult -> Html Msg
viewPermissions currentUserName permissions =
    if permissions == [] then
        div [] [ text "Only you can see this item." ]
    else
        let
            isEditable =
                permissions
                    |> List.any (\pr -> pr.username == currentUserName && pr.permission.write)

            sortByNameAndPerm a b =
                if a.username == currentUserName then
                    LT
                else if b.username == currentUserName then
                    GT
                else
                    compare a.username b.username
        in
        div [ class "container mt-1" ]
            (permissions
                |> List.sortWith sortByNameAndPerm
                |> List.map (\pr -> viewPermission (pr.username == currentUserName) isEditable pr.username pr.permission)
            )


viewPermission : Bool -> Bool -> String -> Permission -> Html Msg
viewPermission isMe isEditable username permission =
    div [ class "row py-2 border-bottom" ]
        [ div [ class "col" ]
            [ Icon.user
            , text " "
            , text username
            , if isMe then
                text " (you)"
              else
                text ""
            ]
        , div [ class "col-3 text-right" ]
            [ if isEditable && not isMe then
                viewPermissionDropdown username permission
              else
                text <| permissionDesc permission
            ]
        ]


viewPermissionDropdown : String -> Permission -> Html Msg
viewPermissionDropdown username permission =
    div [ class "dropdown" ]
        [ button [ class "btn btn-sm btn-outline-secondary dropdown-toggle", style "min-width" "7em", type_ "button", attribute "data-toggle" "dropdown" ]
            [ permissionDesc permission |> text
            , text " "
            , span [ class "caret" ] []
            ]
        , div [ class "dropdown-menu text-nowrap" ]
            [ a [ class "dropdown-item", href "", onClick (ShareWithUser "read-only" username "") ] [ text "Read-only: can view but not modify" ]
            , a [ class "dropdown-item", href "", onClick (ShareWithUser "read/write" username "") ] [ text "Read-write: can view, edit, and delete" ]
            , a [ class "dropdown-item", href "", onClick (ShareWithUser "none" username "") ] [ text "Remove access" ]
            ]
        ]


permissionDesc : Permission -> String
permissionDesc permission =
    if permission.read then
        if permission.write then
            "read/write"
        else
            "read-only"
    else
        "none"



---- PORTS ----


type alias FileToUpload =
    { name : String
    , size : Int
    , type_ : String
    }


fileDecoder : Decoder FileToUpload
fileDecoder =
    Decode.succeed FileToUpload
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "size" Decode.int
        |> Pipeline.required "type" Decode.string


port fileUploadOpenBrowser : (String, String) -> Cmd msg


port fileUploadFileSelected : (String -> msg) -> Sub msg


port fileUploadDone : (String -> msg) -> Sub msg



---- HELPER FUNCTIONS ----


numItems : Model -> Int
numItems (Model {contents}) =
    case List.Extra.uncons contents of
        Nothing ->
            0

        Just (first, rest) ->
            if first.name == ".. (previous)" then
                List.length rest
            else
                List.length contents


getSelected : Model -> List FileResult
getSelected (Model { selectedPaths, contents }) =
    case selectedPaths of
        Nothing ->
            []

        Just paths ->
            List.filter (\f -> List.member f.path paths) contents