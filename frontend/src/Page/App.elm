module Page.App exposing (Model, Msg(..), init, toSession, update, view)

import Session exposing (Session)
import App exposing (App, AppRun)
import Agave
import PlanB
--import Sample exposing (Sample, SampleFile, SampleGroup)
import Cart
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onCheck, onInput)
import Http
import RemoteData exposing (RemoteData(..))
import Route
import Page exposing (viewSpinner, viewSpinnerCentered, viewDialog)
import Json.Decode as Decode
import Json.Encode as Encode
import Task exposing (Task)
--import List.Extra
--import String.Extra
import Maybe exposing (withDefault)
import Dict exposing (Dict)
import RemoteFile exposing (File)
import FileBrowser
import Icon
import Error



---- MODEL ----


type alias Model =
    { session : Session
    , apps : RemoteData Http.Error (App, Agave.App)
    , inputs : Dict String String
    , parameters : Dict String String
    , settings : Dict String String
--    , selectedCartId : Maybe Int
    , files : RemoteData Http.Error (List File)
--    , sampleGroups : List SampleGroup
    , showRunDialog : Bool
    , cartDialogInputId : Maybe String
    , dialogError : Maybe String
    , filterFileFormat : String
    , inputId : Maybe String
    , fileBrowser : FileBrowser.Model
    }


init : Session -> String -> ( Model, Cmd Msg )
init session term =
    let
        loadApp =
            (case String.toInt term of
                Just val ->
                    App.fetch val

                Nothing ->
                    App.fetchByName term
            ) |> Http.toTask

        token =
            Session.token session

        loadAppFromAgave name =
            Agave.getApp token name |> Http.toTask |> Task.map .result

        loadAppFromPlanB name =
            PlanB.getApp token name |> Http.toTask |> Task.map .result

        loadAppFromProvider app = --TODO add more abstraction/types for provider in dedicated module
            if isPlanB app then
                loadAppFromPlanB
            else
                loadAppFromAgave

        fileBrowserConfig =
            { showMenuBar = True
            , showNewFolderButton = False
            , showUploadFileButton = False
            , allowDirSelection = True
            , allowMultiSelection = True
            , allowFileViewing = False
            , homePath = Nothing
            }
    in
    ( { session = session
      , apps = Loading
      , inputs = Dict.empty
      , parameters = Dict.empty
      , settings = Dict.empty
--      , selectedCartId = Nothing -- Current
      , files = NotAsked
--      , sampleGroups = []
      , showRunDialog = False
      , cartDialogInputId = Nothing
      , dialogError = Nothing
      , filterFileFormat = "All Formats"
      , inputId = Nothing
      , fileBrowser = FileBrowser.init session (Just fileBrowserConfig)
      }
      , loadApp
            |> Task.andThen
                (\app ->
                    (loadAppFromProvider app) app.name
                        |> Task.andThen
                            (\agaveApp ->
                                Task.succeed (app, agaveApp)
                            )
                )
            |> Task.attempt GetAppCompleted
    )


toSession : Model -> Session
toSession model =
    model.session


isPlanB : App -> Bool
isPlanB app =
    app.provider == "plan-b"


defaultBatchQueue =
    "normal"


defaultMaxRunTime =
    "12:00:00"



-- UPDATE --


type Msg
    = GetAppCompleted (Result Http.Error (App, Agave.App))
    | SetInput InputSource String String
    | SetParameter String String
    | SetSetting String String
    | RunJob
    | RunJobCompleted (Result Http.Error (Agave.Response Agave.JobStatus))
    | ShareJobCompleted (Result Http.Error (Agave.Response Agave.JobStatus))
    | AppRunCompleted (Result Http.Error AppRun)
    | CloseRunDialog
    | OpenFileBrowserDialog String
    | CloseFileBrowserDialog
    | OpenCart String
    | LoadCartCompleted (Result Http.Error (List File))
--    | SelectCart (Maybe Int)
    | CloseCartDialog
    | CancelCartDialog
    | FilterByFileFormat String
    | CartMsg Cart.Msg
    | FileBrowserMsg FileBrowser.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        token =
            Session.token model.session
    in
    case msg of
        GetAppCompleted (Ok (app, agaveApp)) ->
            let
                defaultInputs l =
                    l |> List.map (\input -> (input.id, default input.value.default)) |> Dict.fromList

                defaultParams l =
                    l |> List.map (\param -> (param.id, default param.value.default)) |> Dict.fromList

                defaultSettings =
                    [ ("batchQueue", agaveApp.defaultQueue), ("maxRunTime", agaveApp.defaultMaxRunTime) ] |> Dict.fromList

                default val =
                    case val of
                        Agave.StringValue s ->
                            s

                        Agave.ArrayValue arr ->
                            String.join ";" arr

                        Agave.BoolValue bool ->
                            if bool then
                                "true"
                            else
                                "false"

                        Agave.NumberValue num ->
                            String.fromFloat num
            in
            ( { model
                | apps = Success (app, agaveApp)
                , inputs = defaultInputs agaveApp.inputs
                , parameters = defaultParams agaveApp.parameters
                , settings = defaultSettings
              }
            , Cmd.none
            )

        GetAppCompleted (Err error) -> --TODO
            ( { model | apps = Failure error }, Error.redirectLoadError error (Session.navKey model.session) )

        SetInput source id value ->
            let
                curValue =
                    Dict.get id model.inputs |> Maybe.withDefault ""

                newValue =
--                    if source == "syndicate" then
--                        if model.app.provider == "plan-b" then
--                            String.split ";" value |> List.map (\s -> "hsyn:///gbmetagenomes/" ++ s) |> String.join ";" --FIXME hardcoded
--                        else
--                            String.split ";" value |> List.map (\s -> "https://www.imicrobe.us/syndicate/download/gbmetagenomes/fs/" ++ s) |> String.join ";" --FIXME hardcoded and duplicated in config.json
                    if source == CYVERSE && curValue /= "" then
                        curValue ++ ";" ++ value
                    else
                        value

                newInputs =
                    Dict.insert id newValue model.inputs

--          Verify file types FIXME
--                exts =
--                    String.split ";" value |> List.map (\a -> String.split "." a |> List.reverse |> List.head)
--
--                aliases =
--                    List.map .alias_ model.app.app_data_types |> List.concatMap (String.split ",")
--
--                isSupported ext =
--                    List.member ext aliases
----                    case ext of
----                        Nothing -> False
----                        Just ext -> List.member ext aliases
--
--                unsupportedExt =
--                    List.filter isSupported aliases
--
--                _ = Debug.log "unsupportedExt" unsupportedExt
--
--                _ = Debug.log "aliases" aliases
--
--                _ = Debug.log "ext" exts
            in
            ( { model | inputs = newInputs, inputId = Nothing }, Cmd.none )

        SetParameter id value ->
            let
                newParams =
                    Dict.insert id value model.parameters
            in
            ( { model | parameters = newParams }, Cmd.none )

        SetSetting id value ->
            let
                newSettings =
                    Dict.insert id value model.settings
            in
            ( { model | settings = newSettings }, Cmd.none )

        RunJob -> --TODO messy, clean this up
            case model.apps of
                Success (app, agaveApp) ->
                    if Agave.validInputs agaveApp model.inputs then
                        let
                            irodsToAgave path = -- convert IRODS paths to Agave paths
                                if String.contains "/iplant/home" path then
                                    String.replace "/iplant/home" "" path -- replace all instances (multiple paths separated by semicolon)
                                else
                                    path

                            jobInputs = --FIXME move into encoder like with parameters
                                Dict.toList model.inputs
                                    |> List.map (\(k, v) -> (k, irodsToAgave v))
                                    |> List.map (\(k, v) -> Agave.JobInput k (if v == "" then [] else String.split ";" v))

                            jobParameters =
                                Agave.encodeJobParameterValues model.parameters agaveApp.parameters

                            jobName =
                                "PlanetMicrobe " ++ app.name --FIXME should be a user-inputted value?

                            jobRequest =
                                { name = jobName
                                , app_id = app.name
                                , archive = True
                                , archiveOnAppError = True
                                , inputs = jobInputs
                                , parameters = jobParameters
                                , notifications = []
                                }

                            jobSettings =
                                Dict.toList model.settings

                            sendAppRun =
                                App.run token app.id (Agave.encodeJobRequest jobRequest jobSettings |> Encode.encode 0) |> Http.send AppRunCompleted

                            launchApp =
                                if isPlanB app then
                                    PlanB.launchJob token jobRequest
                                else
                                    Agave.launchJob token jobRequest jobSettings
                        in
                        ( { model | showRunDialog = True }
                        , Cmd.batch
                            [ launchApp |> Http.send RunJobCompleted
                            , sendAppRun
                            ]
                        )
                    else
                        ( { model | showRunDialog = True, dialogError = Just "Please enter the required inputs (see asterisk)." }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RunJobCompleted (Ok response) ->
            case model.apps of
                Success (app, _) ->
                    let
                        shareJob =
                            if isPlanB app then
                                PlanB.shareJob
                            else
                                Agave.shareJob
                    in
                    ( model
                    , Cmd.batch
                        [ Route.replaceUrl (Session.navKey model.session) (Route.Job response.result.id)
                        , shareJob token response.result.id "planetmicrobe" "READ" |> Http.send ShareJobCompleted
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        RunJobCompleted (Err error) ->
            let
                errorMsg =
                    case error of
                        Http.BadStatus response ->
                            case response.status.code of
                                412 ->
                                    Just "This app is currently unavailable due to CyVerse maintenance. Please try again later."

                                _ ->
                                    case Decode.decodeString Agave.decoderJobError response.body of
                                        Ok result ->
                                            Just result.message

                                        Err _ ->
                                            Just response.body

                        _ -> Just (Error.toString error)
            in
            ( { model | dialogError = errorMsg }, Cmd.none )

        ShareJobCompleted _ ->
            ( model, Cmd.none )

        AppRunCompleted (Ok _) ->
            ( model, Cmd.none )

        AppRunCompleted (Err _) -> --TODO
            ( model, Cmd.none )

        CloseRunDialog ->
            ( { model | showRunDialog = False, dialogError = Nothing }, Cmd.none )

        OpenFileBrowserDialog inputId ->
            let
                (subModel, subCmd) =
                    FileBrowser.update model.session FileBrowser.RefreshPath model.fileBrowser
            in
            ( { model | inputId = Just inputId, fileBrowser = subModel }, Cmd.map FileBrowserMsg subCmd )

        CloseFileBrowserDialog ->
            ( { model | inputId = Nothing }, Cmd.none )

        OpenCart inputId ->
            let
                idList =
                    model.session |> Session.getCart |> Cart.toList

                cmd =
                    if model.files == NotAsked then
                        RemoteFile.fetchSome idList |> Http.toTask |> Task.attempt LoadCartCompleted
                    else
                        Cmd.none
            in
            ( { model | cartDialogInputId = Just inputId }, cmd )

        LoadCartCompleted (Ok files) ->
            ( { model | files = Success files }, Cmd.none )

        LoadCartCompleted (Err error) -> --TODO show error to user
            ( { model | files = Failure error }, Cmd.none )

        CloseCartDialog ->
            let
                selectedIds =
                    model.session |> Session.getCart |> Cart.selectedToList

                --filterOnFormat file =
                --    let
                --        fileFormat =
                --            String.toLower file.format
                --
                --        filterFormat =
                --            String.toLower model.filterFileFormat
                --    in
                --    if (List.member file.sampleId selectedIds && (fileFormat == filterFormat || filterFormat == "all formats") ) then
                --        Just file.url
                --    else
                --        Nothing

                filesStr =
                    model.files
                        |> RemoteData.toMaybe
                        |> Maybe.withDefault []
                        --|> List.filterMap filterOnFormat
                        |> List.filter (\f -> List.member f.id selectedIds)
                        |> List.map .url
                        |> String.join ";"
--                    case model.selectedCartId of
--                        Nothing -> -- Current
--                            List.filterMap filterOnType model.files |> String.join ";"
--
--                        Just id ->
--                            model.sampleGroups
--                                |> List.filter (\g -> g.sample_group_id == id)
--                                |> List.map .samples
--                                |> List.concat
--                                |> List.map .sample_files
--                                |> List.concat
--                                |> List.filterMap filterOnType
--                                |> Set.fromList -- remove duplicates
--                                |> Set.toList
--                                |> String.join ";"

                cmd =
                    SetInput CYVERSE (withDefault "" model.cartDialogInputId) filesStr
            in
            update cmd { model | cartDialogInputId = Nothing }

        CancelCartDialog ->
            ( { model | cartDialogInputId = Nothing }, Cmd.none )

--        SelectCart maybeId ->
--            let
--                newCart =
--                    case maybeId of
--                        Nothing -> -- Current
--                            Cart.init session.cart Cart.Selectable
--
--                        Just id ->
--                            let
--                                cart =
--                                    model.sampleGroups
--                                        |> List.filter (\g -> g.sample_group_id == id)
--                                        |> List.map .samples
--                                        |> List.concat
--                                        |> List.map .sample_id
--                                        |> Set.fromList
--                                        |> Data.Cart.Cart
--                            in
--                            Cart.init cart Cart.Selectable
--            in
--            ( { model | selectedCartId = maybeId, cart = newCart }, Cmd.none )

        FilterByFileFormat format ->
            ( { model | filterFileFormat = format }, Cmd.none )

        CartMsg subMsg ->
            let
                newCart =
                    Cart.update subMsg (Session.getCart model.session)

                newSession =
                    Session.setCart model.session newCart
            in
            ( { model | session = newSession }, Cmd.none )

        FileBrowserMsg subMsg ->
            let
                ( newFileBrowser, subCmd ) =
                    FileBrowser.update model.session subMsg model.fileBrowser
            in
            ( { model | fileBrowser = newFileBrowser }, Cmd.map FileBrowserMsg subCmd )


type InputSource
    = CYVERSE
    | SYNDICATE
    | UI



-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "container" ]
        (case model.apps of
            Success (app, agaveApp) ->
                let
                    body =
                        if not app.is_active then
                            div [ class "alert alert-info" ]
                                [ text "This app is no longer available." ]
                        else if app.is_maintenance then
                            div [ class "alert alert-info" ]
                                [ text "This app is currently unavailable due to maintenance." ]
                        else
                            div []
                                [ viewApp app agaveApp model.inputs model.parameters model.settings
                                , div [ class "text-center mt-3 mb-5" ]
                                    [ hr [] []
                                    , button [ class "btn btn-primary btn-lg mt-5 w-25", onClick RunJob ] [ text "Run" ]
                                    ]
                                ]

                    dialog =
                        if model.inputId /= Nothing then
                            viewFileBrowserDialog model.fileBrowser (model.inputId |> Maybe.withDefault "") False
                        else if model.showRunDialog then
                            viewRunDialog model
                        else if model.cartDialogInputId /= Nothing then
                            viewCartDialog model
                        else
                            text ""
                in
                [ Page.viewTitle "App" app.name
                , body
                , dialog
                ]

            Failure error ->
                [ Error.view error False ]

            Loading ->
                [ viewSpinnerCentered ]

            NotAsked ->
                [ text "" ]
        )


viewApp : App -> Agave.App -> Dict String String -> Dict String String -> Dict String String -> Html Msg
viewApp app agaveApp inputs parameters settings =
    let
        viewInputs =
            case agaveApp.inputs of
                [] ->
                    div [] [ text "None" ]

                _  ->
                    table [ class "table" ]
                        [ tbody []
                            (List.map (zipWithValue inputs .id) agaveApp.inputs |> List.map viewAppInput)
                        ]

        viewParameters =
            case agaveApp.parameters of
                [] ->
                    div [] [ text "None" ]

                _  ->
                    table [ class "table" ]
                        [ tbody []
                            (List.map (zipWithValue parameters .id) agaveApp.parameters |> List.map viewAppParameter)
                        ]

        zipWithValue values id item =
            case Dict.get (id item) values of
                Nothing ->
                    ( item, "" )

                Just val ->
                    ( item, val )
    in
    div []
    [ table [ class "table table-borderless table-sm" ]
        [ tr []
            [ th [] [ text "Name" ]
            , td [] [ text app.name ]
            ]
        , tr []
            [ th [] [ text "Description" ]
            , td [] [ text (if agaveApp.longDescription /= "" then agaveApp.longDescription else agaveApp.shortDescription) ]
            ]
        , tr []
            [ th [] [ text "Help" ]
            , td [] [ a [ href agaveApp.helpURI, target "_blank" ] [ text agaveApp.helpURI ] ]
            ]
        , tr []
            [ th [] [ text "Version" ]
            , td [] [ text agaveApp.version ]
            ]
--        , tr []
--            [ th [] [ text "Tags" ]
--            , td [] [ text (List.map .value app.app_tags |> List.sort |> String.join ", ") ]
--            ]
        ]
    , br [] []
    , Page.viewTitle2 "Inputs" False
    , viewInputs
    , br [] []
    , Page.viewTitle2 "Parameters" False
    , viewParameters
    , br [] []
    , if not (isPlanB app) then
        div []
            [ Page.viewTitle2 "Settings" False
            , viewSettings settings
            ]
      else
        text ""
    ]


viewAppInput : (Agave.AppInput, String) -> Html Msg
viewAppInput input =
    let
        agaveAppInput = Tuple.first input

        val = Tuple.second input

        id = agaveAppInput.id

        label =
            if agaveAppInput.value.required then
                agaveAppInput.details.label ++ " *"
            else
                agaveAppInput.details.label

        browserButton lbl msg =
            button [ class "btn btn-outline-secondary btn-sm text-nowrap", style "height" "2.75em", onClick msg ]
                [ Icon.cloud
                , text " "
                , text lbl
                ]

--        syndicateButton =
            -- mdb changed 6/27/18 -- show RefSeq button in all apps, not just Libra
--            if List.member "syndicate" agaveAppInput.semantics.ontology then
--                browserButton "GenBank" (OpenFileBrowser "syndicate" id)
--            else
--                text ""
    in
    tr []
    [ th [ class "w-25" ] [ text label ]
    , td [ style "min-width" "40vw" ]
        [ div [ style "display" "flex" ]
            [ textarea [ class "form-control mr-2", rows 1, name id, value val, onInput (SetInput UI id) ] []
            , browserButton "Data Store" (OpenFileBrowserDialog id)
--            , syndicateButton
            , button [ class "btn btn-outline-secondary btn-sm text-nowrap ml-2", style "height" "2.75em", onClick (OpenCart id) ]
                [ Icon.shoppingCart
                , text " Cart"
                ]
            ]
        ]
    , td [] [ text agaveAppInput.details.description ]
    ]


viewAppParameter : (Agave.AppParameter, String) -> Html Msg
viewAppParameter input =
    let
        param = Tuple.first input

        val = Tuple.second input

        id = param.id

        defaultInput len =
            Html.input [ class "form-control", type_ "text", size len, name id, value val, onInput (SetParameter id) ] []

        checkbox =
            label []
                [ Html.input [ type_ "checkbox", checked (val == "true"), onCheck (Encode.bool >> Encode.encode 0 >> (SetParameter id)) ] []
                ]

        interface =
            case param.value.type_ of
                "number" ->
                    defaultInput 10

                "bool" ->
                    checkbox

                "flag" ->
                    checkbox

                "enumeration" ->
                    case param.value.enum_values of
                        Nothing -> -- an error in the parameter definition
                            defaultInput 40

                        Just enum ->
                            select [ onInput (SetParameter id) ]
                                (enum
                                    |> List.map (List.head >> withDefault ("error", "error"))
                                    |> List.map (\(value_, label) -> option [ value value_, selected (value_ == val) ] [ text label ])
                                )

                _ ->
                    defaultInput 40

        -- Hide parameters with ID's that start with double-underscore (requested by Ken/Josh)
        hidden =
            if (not param.value.visible) || String.startsWith "__" param.id then
                [ style "display" "none" ]
            else
                []
    in
    tr hidden
    [ th [ class "w-25" ] [ text param.details.label ]
    , td [ class "w-25 text-nowrap" ] [ interface ]
    , td [] [ text param.details.description ]
    ]


viewSettings : Dict String String -> Html Msg
viewSettings settings =
    let
        batchQueue =
            settings |> Dict.get "batchQueue" |> Maybe.withDefault defaultBatchQueue

        maxRunTime =
            settings |> Dict.get "maxRunTime" |> Maybe.withDefault defaultMaxRunTime
    in
    table [ class "table" ]
        [ tbody []
            [ tr []
                [ th [ class "w-25" ] [ text "Queue" ]
                , td [ class "text-nowrap" ]
                    [ select [ onInput (SetSetting "batchQueue") ]
                        [ option [ value "normal", selected (batchQueue == "normal") ] [ text "normal" ]
                        , option [ value "skx-normal", selected (batchQueue == "skx-normal") ] [ text "high memory" ]
                        ]
                    ]
                , td [] [ text "The queue for the job (note that the high memory queue is often much slower)" ]
                ]
            , tr []
                [ th [ class "w-25" ] [ text "Time limit" ]
                , td [ class "text-nowrap" ]
                    [ Html.input [ class "form-control", type_ "text", size 10, value maxRunTime, onInput (SetSetting "maxRunTime") ] [] ]
                , td [] [ text "The maximum run time allowed in HH:MM:SS" ]
                ]
            ]
        ]


--runDialogConfig : Model -> Dialog.Config Msg
--runDialogConfig model =
--    let
--        content =
--            case model.dialogError of
--                Nothing ->
--                    spinner
--
--                Just error ->
--                    div [ class "alert alert-danger" ]
--                        [ p [] [ text "An error occurred:" ]
--                        , p [] [ text error ]
--                        ]
--
--        footer =
--            case model.dialogError of
--                Nothing -> Just (div [] [ text " " ])
--
--                _ ->
--                    Just
--                        (button
--                            [ class "btn btn-default"
--                            , onClick CloseRunDialog
--                            ]
--                            [ text "OK" ]
--                        )
--    in
--    { closeMessage = Nothing
--    , containerClass = Nothing
--    , header = Just (h3 [] [ text "Submitting Job" ])
--    , body = Just content
--    , footer = footer
--    }


viewRunDialog : Model -> Html Msg
viewRunDialog model =
    let
        body =
            case model.dialogError of
                Nothing ->
                    viewSpinner

                Just error ->
                    div [ class "alert alert-danger" ]
                        [ p [] [ text "An error occurred:" ]
                        , p [] [ text error ]
                        ]

        footer =
            if model.dialogError == Nothing then
                div [] [ text " " ]
            else
                button
                    [ class "btn btn-outline-secondary"
                    , onClick CloseRunDialog
                    ]
                    [ text "OK" ]
    in
    viewDialog "Submitting Job"
        [ body ]
        [ footer ]
        CloseRunDialog


--cartDialogConfig : Model -> Dialog.Config Msg
--cartDialogConfig model =
--    let
--        content =
--            if model.cartLoaded then
--                if Cart.size model.cart > 0 then
--                    viewCart model
--                else
--                    text "The cart is empty"
--            else
--                div [ class "center" ]
--                    [ div [ class "padded-xl spinner" ] [] ]
--
--        header =
--            h3 []
--                [ text "Cart "
--                , if model.sampleGroups /= [] then
--                    viewCartDropdown model.selectedCartId model.sampleGroups
--                  else
--                    text ""
--                ]
--
--        closeButton =
--            button [ class "btn btn-default float-right margin-right", onClick CancelCartDialog ] [ text "Close" ]
--
--        empty =
--            Cart.size model.cart == 0
--
--        footer =
--            if not model.cartLoaded || empty then
--                closeButton
--            else
--                div [ style [("display","inline")] ]
--                    [ button [ class "btn btn-default float-left", onClick Cart.SelectAllInCart ]
--                        [ text "Select All" ] |> Html.map CartMsg
--                    , button [ class "btn btn-default float-left", onClick Cart.UnselectAllInCart ]
--                        [ text "Unselect All" ] |> Html.map CartMsg
--                    , div [ class "float-left", style [("margin-left","2em")] ] [ viewFileTypeSelector model ]
--                    , button [ class "btn btn-primary float-right" , onClick CloseCartDialog ]
--                        [ text "Select" ]
--                    , closeButton
--                    ]
--    in
--    { closeMessage = Nothing
--    , containerClass = Nothing
--    , header = Just header
--    , body = Just content
--    , footer = Just footer
--    }


viewCartDialog : Model -> Html Msg
viewCartDialog model =
    let
        cart =
            Session.getCart model.session

        empty =
            Cart.size cart == 0

        body =
            case model.files of
                Success files ->
                    if not empty then
                        div [ style "overflow-y" "auto", style "max-height" "60vh" ]
                            [ Cart.view cart files Cart.Selectable |> Html.map CartMsg ]
                    else
                        text "The cart is empty"
                _ ->
                    viewSpinner

        closeButton =
            button [ class "btn btn-outline-secondary float-right", onClick CancelCartDialog ] [ text "Close" ]

        footer =
            case model.files of
                Success files ->
                    div [ class "d-inline w-100" ]
                        [ button [ class "btn btn-outline-secondary float-left mr-2", onClick Cart.SelectAllInCart ]
                            [ Icon.plus, text " All" ] |> Html.map CartMsg
                        , button [ class "btn btn-outline-secondary float-left", onClick Cart.UnselectAllInCart ]
                            [ Icon.minus, text " All" ] |> Html.map CartMsg
                        --, div [ class "float-left ml-5" ]
                        --    [ viewFileFormatSelector model ]
                        , button [ class "btn btn-primary float-right ml-2" , onClick CloseCartDialog ]
                            [ text "Select" ]
                        , closeButton
                        ]

                _ ->
                    closeButton
    in
    viewDialog "Cart"
        [ body ]
        [ footer ]
        CancelCartDialog


--viewCartDropdown : Maybe Int -> List SampleGroup -> Html Msg
--viewCartDropdown selectedCartId sampleGroups =
--    let
--        mkOption (id, label) =
--            li [] [ a [ onClick (SelectCart id) ] [ text label ] ]
--
--        currentOpt =
--            (Nothing, "Current")
--
--        labels =
--            currentOpt :: (sampleGroups |> List.sortBy .group_name |> List.map (\g -> (Just g.sample_group_id, g.group_name)))
--
--        options =
--            labels |> List.map mkOption
--
--        btnLabel =
--            List.Extra.find (\l -> Tuple.first l == selectedCartId) labels |> Maybe.withDefault currentOpt |> Tuple.second
--    in
--    if sampleGroups == [] then
--        text ""
--    else
--        div [ style [ ("display", "inline-block") ] ]
--            [ div [ class "dropdown margin-right", style [ ("display", "inline-block") ] ]
--                [ button [ class "btn btn-default dropdown-toggle margin-top-bottom", attribute "type" "button", id "dropdownMenu1", attribute "data-toggle" "dropdown", attribute "aria-haspopup" "true", attribute "aria-expanded" "true" ]
--                    [ text btnLabel
--                    , text " "
--                    , span [ class "caret" ] []
--                    ]
--                , ul [ class "dropdown-menu", attribute "aria-labelledby" "dropdownMenu1" ]
--                    options
--                ]
--            ]


--viewCart : Model -> Html Msg
--viewCart model =
--    let
--        samples =
--            case model.selectedCartId of
--                Nothing -> -- Current
--                    model.samples
--
--                Just id ->
--                    model.sampleGroups
--                        |> List.filter (\g -> g.sample_group_id == id)
--                        |> List.map .samples
--                        |> List.concat
--    in
--    div [ class "scrollable-half" ] [ Cart.viewCart model.cart samples |> Html.map CartMsg ]


--viewFileFormatSelector : Model -> Html Msg
--viewFileFormatSelector model =
--    let
--        types =
--            model.files
--                |> List.map .format
--                |> List.map String.Extra.toSentenceCase
--                |> List.Extra.unique
----            case model.selectedCartId of
----                Nothing -> -- Current
----                    model.files
----                        |> List.map (.sample_file_type >> .file_type)
----                        |> Set.fromList
----                        |> Set.toList
----
----                Just id ->
----                    model.sampleGroups
----                        |> List.filter (\g -> g.sample_group_id == id)
----                        |> List.map .samples
----                        |> List.concat
----                        |> List.map .sample_files
----                        |> List.concat
----                        |> List.map (.sample_file_type >> .file_type)
----                        |> Set.fromList
----                        |> Set.toList
--
--        item label =
--            a [ class "dropdown-item", href "", onClick (FilterByFileFormat label) ] [ text label ]
--
--        selectedType =
--            if model.filterFileFormat == "All Formats" then
--                "File Format"
--            else
--                model.filterFileFormat
--    in
--    div [ class "dropup" ]
--        [ button
--            [ class "btn btn-outline-secondary dropdown-toggle", id "dropdownMenuButton",
--                type_ "button", attribute "data-toggle" "dropdown", attribute "aria-haspopup" "true", attribute "aria-expanded" "false"
--            ]
--            [ text selectedType
--            , text " "
--            , span [ class "caret" ] []
--            ]
--        , div [ class "dropdown-menu", style "overflow-y" "scroll", style "max-height" "200px", attribute "aria-labelledby" "dropdownMenuButton" ]
--            ("All Formats" :: types |> List.map item)
--        ]


--fileBrowserDialogConfig : FileBrowser.Model -> String -> Bool -> Dialog.Config Msg
--fileBrowserDialogConfig fileBrowser inputId isBusy =
--    let
--        content =
--            if isBusy then
--                spinner
--            else
--                div [ class "scrollable", style [ ("min-height","50vh"),("max-height","50vh") ] ]
--                    [ FileBrowser.view fileBrowser |> Html.map FileBrowserMsg ]
--
--        footer =
--            let
--                selectedFilepaths =
--                    FileBrowser.getSelected fileBrowser |> List.map .path |> String.join ";"
--            in
--            div []
--                [ button [ class "btn btn-default float-left", onClick CloseFileBrowserDialog ] [ text "Cancel" ]
--                , button [ class "btn btn-primary", onClick (SetInput CYVERSE inputId selectedFilepaths) ] [ text "Select" ]
--                ]
--    in
--    { closeMessage = Just CloseFileBrowserDialog
--    , containerClass = Just "wide-modal-container"
--    , header = Just (h3 [] [ text "Add Files" ])
--    , body = Just content
--    , footer = Just footer
--    }

-- TODO move into module.  This is our own Boostrap modal since elm-dialog has not yet been ported to Elm 0.19
viewFileBrowserDialog : FileBrowser.Model -> String -> Bool -> Html Msg
viewFileBrowserDialog fileBrowser inputId isBusy =
    let
        title =
            "Select Files"

        body =
            if isBusy then
                viewSpinner
            else
                div [ style "min-height" "50vh", style "max-height" "50vh", style "overflow-y" "auto" ]
                    [ FileBrowser.view fileBrowser |> Html.map FileBrowserMsg ]

        footer =
            let
                selectedFilepaths =
                    FileBrowser.getSelected fileBrowser |> List.map .path |> String.join ";"
            in
            div []
                [ button [ class "btn btn-outline-secondary float-left mr-2", onClick CloseFileBrowserDialog ] [ text "Cancel" ]
                , button [ class "btn btn-primary", onClick (SetInput CYVERSE inputId selectedFilepaths) ] [ text "Select" ]
                ]

        closeMsg =
            CloseFileBrowserDialog

        width =
            "40vw"
    in
    div []
        [ div [ class "modal fade show", tabindex -1, style "display" "block", attribute "role" "dialog" ]
            [ div [ class "modal-dialog", attribute "role" "document", style "min-width" width ]
                [ div [ class "modal-content" ]
                    [ div [ class "modal-header" ]
                        [ h5 [ class "modal-title" ] [ text title ]
                        , button [ type_ "button", class "close", onClick closeMsg ] [ span [] [ text (String.fromChar (Char.fromCode 215)) ] ]
                        ]
                    , div [ class "modal-body" ] [ body ]
                    , div [ class "modal-footer" ] [ footer ]
                    ]
                ]
            ]
        , div [ class "modal-backdrop fade show" ] []
        ]
