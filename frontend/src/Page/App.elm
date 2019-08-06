module Page.App exposing (Model, Msg(..), init, toSession, update, view)

import Session exposing (Session)
import App exposing (App, AppRun)
import Agave
import Sample exposing (Sample)--, SampleFile, SampleGroup)
import Cart
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onCheck, onInput)
import Http
import Route
import Page exposing (viewSpinner, viewDialog)
import Json.Decode as Decode
import Json.Encode as Encode
import Task exposing (Task)
--import View.Cart as Cart
--import View.Spinner exposing (spinner)
--import View.FileBrowser as FileBrowser
import List.Extra
import String.Extra
import Maybe exposing (withDefault)
import Dict exposing (Dict)
import File exposing (File)
import FileBrowser
import Icon
import Error
--import Debug exposing (toString)



---- MODEL ----


type alias Model =
    { session : Session
    , appId : Int
    , app : Maybe App
    , agaveApp : Maybe Agave.App
    , inputs : Dict String String
    , parameters : Dict String String
    , cartLoaded : Bool
--    , selectedCartId : Maybe Int
    , samples : List Sample
    , files : List File
--    , sampleGroups : List SampleGroup
    , showRunDialog : Bool
    , cartDialogInputId : Maybe String
    , dialogError : Maybe String
    , filterFileFormat : String
    , inputId : Maybe String
    , fileBrowser : FileBrowser.Model
    }


init : Session -> Int -> ( Model, Cmd Msg )
init session id =
    let
        -- Load page - Perform tasks to load the resources of a page
        loadApp =
            App.fetch id |> Http.toTask

        loadAppFromAgave name =
--            Agave.getApp session.token name |> Http.toTask |> Task.map .result
            Agave.getApp (Session.token session) name |> Http.toTask |> Task.map .result

--        loadAppFromPlanB name =
--            Request.PlanB.getApp session.token name |> Http.toTask |> Task.map .result

        loadAppFromProvider app =
            case app.provider of
--                "plan-b" -> loadAppFromPlanB

                _ -> loadAppFromAgave

--        cart =
--            Cart.init session.cart Cart.Selectable

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
      , appId = id
      , app = Nothing
      , agaveApp = Nothing
      , inputs = Dict.empty --inputs agaveApp
      , parameters = Dict.empty --params agaveApp
      , cartLoaded = False
--      , selectedCartId = Nothing -- Current
      , samples = []
      , files = []
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



-- UPDATE --


type Msg
    = GetAppCompleted (Result Http.Error (App, Agave.App))
    | SetInput InputSource String String
    | SetParameter String String
    | RunJob
    | RunJobCompleted (Result Http.Error (Agave.Response Agave.JobStatus))
    | ShareJobCompleted (Result Http.Error (Agave.Response Agave.JobStatus))
    | AppRunCompleted (Result Http.Error AppRun)
    | CloseRunDialog
    | OpenFileBrowserDialog String
    | CloseFileBrowserDialog
    | OpenCart String
    | LoadCartCompleted (Result Http.Error ((List Sample), (List File))) --, (List SampleGroup)))
--    | SelectCart (Maybe Int)
    | CloseCartDialog
    | CancelCartDialog
    | FilterByFileFormat String
    | CartMsg Cart.Msg
    | FileBrowserMsg FileBrowser.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        isPlanB =
            (model.app |> Maybe.map .provider |> Maybe.withDefault "") == "plan-b"
    in
    case msg of
        GetAppCompleted (Ok (app, agaveApp)) ->
            let
                mapInputs l =
                    l |> List.map (\input -> (input.id, (default input.value.default))) |> Dict.fromList

                mapParams l =
                    l |> List.map (\param -> (param.id, default param.value.default)) |> Dict.fromList

                default val =
                    case val of
                        Agave.StringValue s -> s

                        Agave.ArrayValue arr -> String.join ";" arr

                        Agave.BoolValue bool -> ""

                        Agave.NumberValue num -> String.fromFloat num
            in
            ( { model
                | app = Just app
                , agaveApp = Just agaveApp
                , inputs = mapInputs agaveApp.inputs
                , parameters = mapParams agaveApp.parameters
              }
            , Cmd.none
            )

        GetAppCompleted (Err error) -> --TODO
--            let
--                _ = Debug.log "GetAppCompleted" (toString error)
--            in
            ( model, Cmd.none )

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
                newParams = Dict.insert id value model.parameters
            in
            ( { model | parameters = newParams }, Cmd.none )

        RunJob -> --TODO messy, clean this up
            case (model.app, model.agaveApp) of
                (Just app, Just agaveApp) ->
                    let
                        irodsToAgave path = -- convert IRODS paths to Agave paths
                            if String.contains "/iplant/home" path then
                                String.replace "/iplant/home" "" path -- replace all instances (multiple paths separated by semicolon)
                            else
                                path

                        jobInputs =
                            Dict.toList model.inputs
                                |> List.map (\(k, v) -> (k, irodsToAgave v))
                                |> List.map (\(k, v) -> Agave.JobInput k (String.split ";" v))

                        encodeParam id val =
                            case List.filter (\p -> p.id == id) agaveApp.parameters of
                                [ param ] ->
                                    case param.value.type_ of
                                        "number" ->
                                            Agave.NumberValue (String.toFloat val |> Maybe.withDefault 0)

                                        "bool" ->
                                            if val == "true" then
                                                Agave.BoolValue True
                                            else
                                                Agave.BoolValue False

                                        "flag" ->
                                            if val == "true" then
                                                Agave.BoolValue True
                                            else
                                                Agave.BoolValue False

                                        "enumeration" ->
                                            Agave.ArrayValue (String.split ";" val)

                                        _ ->
                                            Agave.StringValue val

                                _ ->
                                    Agave.StringValue val

                        jobParameters =
                            Dict.toList model.parameters |> List.map (\(k, v) -> Agave.JobParameter k (encodeParam k v))

                        jobName =
                            "iMicrobe " ++ app.name --FIXME should be a user-inputted value?

                        jobRequest =
                            Agave.JobRequest jobName app.name True jobInputs jobParameters []

                        launchAgave =
                            Agave.launchJob (Session.token model.session) jobRequest |> Http.send RunJobCompleted

        --                launchPlanB =
        --                    Request.PlanB.launchJob session.token jobRequest |> Http.send RunJobCompleted

                        sendAppRun =
                            App.run (Session.token model.session) model.appId (Agave.encodeJobRequest jobRequest |> Encode.encode 0) |> Http.send AppRunCompleted

                        launchApp =
        --                    if isPlanB then
        --                        launchPlanB
        --                    else
                                launchAgave
                    in
                    ( { model | showRunDialog = True }, Cmd.batch [ launchApp, sendAppRun ] )

                (_, _) ->
                    ( model, Cmd.none )

        RunJobCompleted (Ok response) ->
            let
                shareJob =
                    Agave.shareJob (Session.token model.session) response.result.id "imicrobe" "READ" |> Http.send ShareJobCompleted

                cmd =
                    ( (Route.replaceUrl (Session.navKey model.session) (Route.Job response.result.id)
                        :: (if not isPlanB then [ shareJob ] else []))
                        |> Cmd.batch
                    )
            in
            ( model, cmd )

        RunJobCompleted (Err error) ->
            let
--                _ = Debug.log "error" (toString error)

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

        AppRunCompleted (Ok response) ->
            ( model, Cmd.none )

        AppRunCompleted (Err error) ->
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
                    if model.cartLoaded then
                        Cmd.none
                    else if idList == [] then -- current cart is empty
                        Task.attempt LoadCartCompleted <|
                            Task.map2 (\samples files -> (samples, files))
                                (Task.succeed [])
                                (Task.succeed [])
--                            Task.map3 (\samples files sampleGroups -> (samples, files, sampleGroups))
--                                (Task.succeed [])
--                                (Task.succeed [])
--                                (Request.SampleGroup.list session.token |> Http.toTask) -- load samples & files for saved carts
                    else
                        Task.attempt LoadCartCompleted <|
                            Task.map2 (\samples files -> (samples, files))
                                (Sample.fetchSome idList |> Http.toTask) -- load samples for current cart
                                (File.fetchAllBySamples idList |> Http.toTask) -- load files for current cart
--                            Task.map3 (\samples files sampleGroups -> (samples, files, sampleGroups))
--                                (Sample.fetchSome session.token id_list |> Http.toTask) -- load samples for current cart
--                                (Request.Sample.files session.token id_list |> Http.toTask) -- load files for current cart
--                                (Request.SampleGroup.list session.token |> Http.toTask) -- load samples & files for saved carts
            in
            ( { model | cartDialogInputId = Just inputId }, cmd )

        LoadCartCompleted (Ok (samples, files)) -> --(Ok (samples, files, sampleGroups)) ->
--            ( { model | samples = samples, files = files, sampleGroups = sampleGroups, cartLoaded = True }, Cmd.none )
            ( { model | cartLoaded = True, samples = samples, files = files }, Cmd.none )

        LoadCartCompleted (Err error) -> --TODO show error to user
--            let
--                _ = Debug.log "LoadCartCompleted" (toString error)
--            in
            ( model, Cmd.none )

        CloseCartDialog ->
            let
                selectedIds =
                    model.session |> Session.getCart |> Cart.selectedToList

                filterOnFormat file =
                    let
                        fileFormat =
                            String.toLower file.format

                        filterFormat =
                            String.toLower model.filterFileFormat
                    in
                    if (List.member file.sampleId selectedIds && (fileFormat == filterFormat || filterFormat == "all formats") ) then
                        Just file.url
                    else
                        Nothing

                filesStr =
                    List.filterMap filterOnFormat model.files |> String.join ";"
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
--                _ = Debug.log "App.CartMsg" (toString subMsg)

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
    case (model.app, model.agaveApp) of
        (Just app, Just agaveApp) ->
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
                            [ viewApp app agaveApp model.inputs model.parameters
                            , div [ class "text-center" ]
                                [ hr [] []
                                , button [ class "btn btn-primary btn-lg", onClick RunJob ] [ text "Run" ]
                                ]
                            ]
            in
            div [ class "container" ]
                [ Page.viewTitle "App" app.name
                , body
                , if model.inputId /= Nothing then
                    viewFileBrowserDialog model.fileBrowser (model.inputId |> Maybe.withDefault "") False
                  else
                    text ""
        --            , Dialog.view
        --                (if model.showRunDialog then
        --                    Just (runDialogConfig model)
        --                 else if model.cartDialogInputId /= Nothing then
        --                    Just (cartDialogConfig model)
--                         else if model.inputId /= Nothing then
--                            Just (fileBrowserDialogConfig model.fileBrowser (model.inputId |> Maybe.withDefault "") False)
        --                 else
        --                    Nothing
        --                )
                , if model.cartDialogInputId /= Nothing then
                    viewCartDialog model
                  else
                    text ""
                ]

        (_, _) ->
            viewSpinner


viewApp : App -> Agave.App -> Dict String String -> Dict String String -> Html Msg
viewApp app agaveApp inputs parameters =
    let
        viewInputs =
            case agaveApp.inputs of
                [] ->
                    div [] [ text "None" ]

                _  ->
                    table [ class "table" ]
                        [ tbody [] (List.Extra.zip agaveApp.inputs (Dict.values inputs) |> List.map viewAppInput)
                        ]

        viewParameters =
            case agaveApp.parameters of
                [] ->
                    div [] [ text "None" ]

                _  ->
                    table [ class "table" ]
                        [ tbody [] (List.Extra.zip agaveApp.parameters (Dict.values parameters) |> List.map viewAppParameter)
                        ]
    in
    div []
    [ table [ class "table table-borderless table-sm" ]
        [ tr []
            [ th [] [ text "Name" ]
            , td [] [ text app.name ]
            ]
        , tr []
            [ th [] [ text "Description" ]
            , td [] [ text agaveApp.shortDescription ]
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
    , Page.viewTitle2 "Inputs" False
    , viewInputs
    , Page.viewTitle2 "Parameters" False
    , viewParameters
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
            button [ class "btn btn-outline-secondary btn-sm", style "height" "2.75em", onClick msg ]
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
    , td []
        [ div [ style "display" "flex" ]
            [ textarea [ class "form-control mr-2", style "width" "30em", rows 1, name id, value val, onInput (SetInput UI id) ] []
            , browserButton "Data Store" (OpenFileBrowserDialog id)
--            , syndicateButton
            , button [ class "btn btn-outline-secondary btn-sm ml-2", style "height" "2.75em", onClick (OpenCart id) ]
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
                [ Html.input [ type_ "checkbox", onCheck (Encode.bool >> Encode.encode 0 >> (SetParameter id)) ] []
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
    , td [ class "text-nowrap" ] [ interface ]
    , td [] [ text param.details.description ]
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
            if model.cartLoaded then
                if not empty then
                    div [ style "overflow-y" "auto", style "max-height" "60vh" ]
                        [ Cart.view cart model.samples Cart.Selectable |> Html.map CartMsg ]
                else
                    text "The cart is empty"
            else
                div [ class "center" ]
                    [ div [] [ text "Loading..." ] ]

        closeButton =
            button [ class "btn btn-outline-secondary float-right", onClick CancelCartDialog ] [ text "Close" ]

        footer =
            if not model.cartLoaded || empty then
                closeButton
            else
                div [ class "d-inline w-100" ]
                    [ button [ class "btn btn-outline-secondary float-left mr-2", onClick Cart.SelectAllInCart ]
                        [ Icon.plus, text " All" ] |> Html.map CartMsg
                    , button [ class "btn btn-outline-secondary float-left", onClick Cart.UnselectAllInCart ]
                        [ Icon.minus, text " All" ] |> Html.map CartMsg
                    , div [ class "float-left ml-5" ]
                        [ viewFileFormatSelector model ]
                    , button [ class "btn btn-primary float-right ml-2" , onClick CloseCartDialog ]
                        [ text "Select" ]
                    , closeButton
                    ]
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


viewFileFormatSelector : Model -> Html Msg
viewFileFormatSelector model =
    let
        types =
            model.files
                |> List.map .format
                |> List.map String.Extra.toSentenceCase
                |> List.Extra.unique
--            case model.selectedCartId of
--                Nothing -> -- Current
--                    model.files
--                        |> List.map (.sample_file_type >> .file_type)
--                        |> Set.fromList
--                        |> Set.toList
--
--                Just id ->
--                    model.sampleGroups
--                        |> List.filter (\g -> g.sample_group_id == id)
--                        |> List.map .samples
--                        |> List.concat
--                        |> List.map .sample_files
--                        |> List.concat
--                        |> List.map (.sample_file_type >> .file_type)
--                        |> Set.fromList
--                        |> Set.toList

        btn label =
            button [ class "btn btn-default", onClick (FilterByFileFormat label) ] [ text label ]

        item label =
            a [ class "dropdown-item", href "", onClick (FilterByFileFormat label) ] [ text label ]

        selectedType =
            case model.filterFileFormat of
                "All Formats" ->
                    "File Format "

                _ ->
                    model.filterFileFormat ++ " "
    in
    div [ class "dropup" ]
        [ button
            [ class "btn btn-outline-secondary dropdown-toggle", id "dropdownMenu1",
                attribute "type" "button", attribute "data-toggle" "dropdown", attribute "aria-haspopup" "true", attribute "aria-expanded" "true"
            ]
            [ text selectedType
            , span [ class "caret" ] []
            ]
        , div [ class "dropdown-menu", style "overflow-y" "scroll", style "max-height" "200px", attribute "aria-labelledby" "dropdownMenu1" ]
            (item "All Formats" :: List.map item types)
        ]


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
                text "Loading..." --spinner
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
