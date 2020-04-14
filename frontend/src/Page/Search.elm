module Page.Search exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (class, classList, style, href, disabled, type_, value, placeholder, target, checked, attribute)
import Html.Events exposing (onClick, onInput, onCheck)
import Http
import RemoteData exposing (RemoteData(..))
import File.Download as Download
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Date exposing (Date)
import DatePicker exposing (DatePicker, DateEvent(..))
import Time exposing (Weekday(..))
import Task
import String.Extra
import List.Extra
import Set
import GMap
import Dict exposing (Dict)
import Route
import Error
import Page exposing (viewBlank, viewDialog)
import Project
import Sample
import Search exposing (SearchResponse, SampleResult, FileResult, SearchResultValue(..), Value(..), Filter, FilterValue(..), SearchTerm, PURL, Distribution, defaultSearchTerm, validFilterValue)
import RemoteFile
import SortableTable
import BarChart
import Cart
import Icon
import Config exposing (dataCommonsUrl)
import Debug exposing (toString)



-- CONSTANTS --


maxResultColWidth =
    "8em"


defaultPageSize =
    20


maxNumPanelOptions =
    4


minNumPanelOptionsForSearchBar =
    100


maxCartSize =
    250


purlDepth =
    "http://purl.obolibrary.org/obo/ENVO_3100031"


--purlDepthMin =
--    "http://purl.obolibrary.org/obo/PMO_00000172"


--purlDepthMax =
--    "http://purl.obolibrary.org/obo/PMO_00000052"


--TODO Need to resolve time zone issue with fields that reference this PURL
--purlDateTimeLocal =
--    "http://purl.obolibrary.org/obo/PMO_00000081"


purlDateTimeISO =
    "http://purl.obolibrary.org/obo/OBI_0001619"


purlDateTimeISOStart =
    "http://purl.obolibrary.org/obo/PMO_00000008"


purlDateTimeISOEnd =
    "http://purl.obolibrary.org/obo/PMO_00000009"


purlBiome =
    "http://purl.obolibrary.org/obo/ENVO_00000428"


purlEnvironmentalFeature =
    "http://purl.obolibrary.org/obo/ENVO_00002297"


purlEnvironmentalMaterial =
    "http://purl.obolibrary.org/obo/ENVO_00010483"


purlLocation = -- Not a real PURL but rather a unique id for this term used in search query string
    "location"


purlProject = -- Not a real PURL but rather a unique id for this term used in search query string
    "project"


purlFileSource = -- Not a real PURL but rather a unique id for this term used in search query string
    "source"


purlFileStrategy = -- Not a real PURL but rather a unique id for this term used in search query string
    "strategy"


purlFileSelection = -- Not a real PURL but rather a unique id for this term used in search query string
    "selection"


purlFileLayout = -- Not a real PURL but rather a unique id for this term used in search query string
    "layout"


-- Required terms that should always appear in the "Sample" filter tab
permanentSampleFilters =
    [ Search.defaultFilter purlLocation "Location"
    , Search.defaultFilter purlDepth "Depth"
    , Search.defaultFilter purlDateTimeISO "Date/Time"
    , Search.defaultFilter purlProject "Project"
    ]


-- Optional terms to start with in the "Sample" filter tab
initialAddedSampleTerms =
    [ purlBiome
    , purlEnvironmentalFeature
    , purlEnvironmentalMaterial
    ]


-- Required terms that should always appear in the "File" filter tab
permanentFileTerms =
    [ purlFileSource
    , purlFileStrategy
    , purlFileSelection
    , purlFileLayout
    ]


-- Removed from "Add Filter" list because they are included in 4D filters already
redundantTerms =
    [ "http://purl.obolibrary.org/obo/OBI_0001620"  -- latitude coordinate measurement datum
    , "http://purl.obolibrary.org/obo/OBI_0001621"  -- longitude coordinate measurement datum
    , "http://purl.obolibrary.org/obo/PMO_00000076" -- latitude coordinate measurement datum start
    , "http://purl.obolibrary.org/obo/PMO_00000077" -- longitude coordinate measurement datum start
    , "http://purl.obolibrary.org/obo/PMO_00000079" -- latitude coordinate measurement datum stop
    , "http://purl.obolibrary.org/obo/PMO_00000078" -- longitude coordinate measurement datum stop
    , "http://purl.obolibrary.org/obo/BFO_0000148"  -- zero-dimensional temporal region
    , "http://purl.obolibrary.org/obo/ENVO_3100031" -- depth of water
    , "http://purl.obolibrary.org/obo/OBI_0001619"  -- specimen collection time measurement datum
    , "http://purl.obolibrary.org/obo/PMO_00000008" -- specimen collection time measurement datum start
    , "http://purl.obolibrary.org/obo/PMO_00000009" -- specimen collection time measurement datum stop
    ]



-- MODEL --


type alias Model =
    { session : Session

    -- Search filter state
    , allTerms: List SearchTerm -- all terms available to search on
    , sampleFilters : List Filter -- sample tab search filters
    , addedSampleFilters : List Filter -- user-added sample tab search filters
    , displayedSampleFilters : List Filter -- user-added sample tab search filters currently displayed in search results
    , fileFilters : List Filter -- file tab search filters
    , dateRangePickers : Dict PURL DateRangePicker -- Datepicker UI elements (there could be datetime fields in addition to start/end in 4D)
    , showParamSearchDropdown : Bool
    , paramSearchInputVal : String

    -- Search result state
    , searchStatus : SearchStatus
    , searchResponse : RemoteData String SearchResponse
    --, sampleResults : RemoteData String (List SampleResult)
    --, fileResults : RemoteData String  (List FileResult)
    --, mapResults : Encode.Value --List MapResult
    --, summaryResults : RemoteData String  (List Distribution)
    --, sampleResultCount : Int
    --, fileResultCount : Int
    , sampleTableState : SortableTable.State
    , fileTableState : SortableTable.State
    , searchTab : String
    , resultTab : String
    , pageNum : Int
    , pageSize : Int
    , showMap : Bool
    , mapLoaded : Bool
    , previousSearchParams : List (String, String)

    -- Dialog states
    , dialogState : DialogState
    }


type SearchStatus
    = SearchNot          -- Idle
    | SearchInit Int     -- Semaphore on init requests to finish
    | SearchPending      -- Pending state to debounce search triggers
    | SearchInProgress   -- Search request in progress
    | SearchError String -- Search request failed


type DialogState
    = NoDialog
    | AddFilterDialog String
    | StringFilterDialog Filter
    | FilterSummaryDialog SearchTerm
    | MessageDialog String


type alias DateRangePicker =
    { start : DatePicker
    , end : DatePicker
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        -- Init requests performed asynchronously -- completion tracked using SearchInit semaphore rather than Task.sequence which is synchronous (slow)
        initRequests =
            [ Sample.fetchAllSearchTerms |> Http.send GetAllSearchTermsCompleted
            , Sample.fetchSearchTerms initialAddedSampleTerms |> Http.send GetSearchTermsCompleted
            , Project.fetchCounts |> Http.send GetProjectCountsCompleted
            , RemoteFile.fetchProperties |> Http.send GetFilePropertiesCompleted
            ]
    in
    (
        { session = session

        -- Search filter state
        , allTerms = [] -- set by GetAllSearchTermsCompleted
        , sampleFilters = permanentSampleFilters
        , addedSampleFilters = [] -- set by GetSearchTermsCompleted
        , displayedSampleFilters = [] -- set by SearchCompleted
        , fileFilters = [] -- set by GetFilePropertiesCompleted
        , dateRangePickers = Dict.empty -- set by InitDatePickers
        , showParamSearchDropdown = False
        , paramSearchInputVal = ""

        -- Search result state
        , searchStatus = SearchInit (List.length initRequests)
        , searchResponse = NotAsked
        --, sampleResults = NotAsked
        --, fileResults = NotAsked
        --, mapResults = Encode.object []
        --, summaryResults = NotAsked
        --, sampleResultCount = 0
        --, fileResultCount = 0
        , sampleTableState = SortableTable.initialState
        , fileTableState = SortableTable.initialState
        , searchTab = "Samples"
        , resultTab = "Samples"
        , pageNum = 0
        , pageSize = defaultPageSize
        , showMap = True
        , mapLoaded = False
        , previousSearchParams = []

        -- Dialog states
        , dialogState = NoDialog
        }
    , Cmd.batch
        ( initRequests ++
            [ Date.today |> Task.perform InitDatePickers
            , GMap.removeMap "" -- workaround for blank map on navigating back to this page
            , GMap.changeMapSettings (GMap.Settings True True False True |> GMap.encodeSettings)
            ]
        )
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 500 InputTimerTick -- milliseconds, for debouncing search triggers
        , GMap.getLocation UpdateLocationFromMap
        , GMap.mapLoaded MapLoaded
        ]


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE --


type Msg
    = GetProjectCountsCompleted (Result Http.Error Distribution)
    | GetFilePropertiesCompleted (Result Http.Error (List (String, Distribution)))
    | GetAllSearchTermsCompleted (Result Http.Error (List SearchTerm))
    | GetSearchTermsCompleted (Result Http.Error (List SearchTerm))
    | ClearFilters
    | AddFilter PURL
    | RemoveFilter PURL
    | OpenDialog DialogState
    | CloseDialog
    | SetParamSearchInput String
    | ShowParamSearchDropdown
    | SetDialogSearchInput String
    | SetSearchFilterValue PURL String
    | SetStringFilterValue PURL String Bool
    | SetFilterValue PURL FilterValue
    | SetSearchTab String
    | SetResultTab String
    | SetSampleSortPos Int
    | SetFileSortPos Int
    | SetPageSize Int
    | SetPageNum Int
    | InputTimerTick Time.Posix
    | Search Int Bool
    | SearchCompleted (Result Http.Error SearchResponse)
    | DownloadSearchCompleted (Result Http.Error String)
    | ToggleMap
    | UpdateLocationFromMap (Maybe GMap.Location)
    | MapLoaded Bool
    | InitDatePickers Date
    | SetStartDatePicker PURL FilterValue DatePicker.Msg
    | SetEndDatePicker PURL FilterValue DatePicker.Msg
    | CartMsg Cart.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateSearchStatus searchStatus =
            case searchStatus of
                SearchInit n ->
                    if n - 1 == 0 then
                        SearchPending
                    else
                        SearchInit (n - 1)

                _ ->
                    SearchPending
    in
    case msg of
        GetProjectCountsCompleted (Ok counts) ->
            let
                projectFilter =
                    Filter { defaultSearchTerm | id = purlProject, label = "Project", distribution = counts } NoValue

                newFilters =
                    Search.updateFilter purlProject projectFilter model.sampleFilters
            in
            ( { model | sampleFilters = newFilters, searchStatus = updateSearchStatus model.searchStatus }, Cmd.none )

        GetProjectCountsCompleted (Err error) ->
            ( { model | searchStatus = SearchError (Error.toString error) }, Cmd.none )

        GetFilePropertiesCompleted (Ok props) ->
            let
                fileFilters =
                    props
                        |> List.map
                            (\(id,dist) ->
                                Filter { defaultSearchTerm | id = id, label = id, distribution = dist } NoValue
                            )
            in
            ( { model | fileFilters = fileFilters, searchStatus = updateSearchStatus model.searchStatus }, Cmd.none )

        GetFilePropertiesCompleted (Err error) ->
            ( { model | searchStatus = SearchError (Error.toString error) }, Cmd.none )

        GetAllSearchTermsCompleted (Ok terms) ->
            ( { model | allTerms = terms, searchStatus = updateSearchStatus model.searchStatus }, Cmd.none )

        GetAllSearchTermsCompleted (Err error) ->
            ( { model | searchStatus = SearchError (Error.toString error) }, Cmd.none )

        GetSearchTermsCompleted (Ok terms) ->
            let
                newFilter term =
                    { term = term
                    , value =
                        case term.type_ of
                            "number" ->
                                RangeValue (String.fromFloat term.min) (String.fromFloat term.max)

                            _ ->
                                NoValue
                    }

                newFilters =
                    List.append model.addedSampleFilters (List.map newFilter terms)
            in
            ( { model | addedSampleFilters = newFilters, searchStatus = updateSearchStatus model.searchStatus }, Cmd.none )

        GetSearchTermsCompleted (Err error) ->
            ( { model | searchStatus = SearchError (Error.toString error) }, Cmd.none )

        ClearFilters ->
            let
                sampleFilters =
                    model.sampleFilters |> List.map Search.resetFilter

                addedSampleFilters =
                    model.addedSampleFilters
                        |> List.filter (\f -> List.member f.term.id initialAddedSampleTerms)
                        |> List.map Search.resetFilter

                fileFilters =
                    model.fileFilters |> List.map Search.resetFilter
            in
            ( { model
                | searchStatus = SearchPending
                , sampleFilters = sampleFilters
                , addedSampleFilters = addedSampleFilters
                , fileFilters = fileFilters
                , sampleTableState = SortableTable.initialState
                , fileTableState = SortableTable.initialState
              }
            , GMap.setLocation Nothing
            )

        AddFilter id ->
            ( { model
                | showParamSearchDropdown = False
                , paramSearchInputVal = ""
                , dialogState = NoDialog
              }
            , Sample.fetchSearchTerms [ id ] |> Http.send GetSearchTermsCompleted
            )

        RemoveFilter id ->
            let
                newFilters =
                    model.addedSampleFilters |> List.filter (\f -> f.term.id /= id)
            in
            ( { model
                | searchStatus = SearchPending
                , addedSampleFilters = newFilters
                , sampleTableState = SortableTable.initialState
                , fileTableState = SortableTable.initialState
              }
            , Cmd.none
            )

        OpenDialog state ->
            ( { model | dialogState = state }, Cmd.none )

        CloseDialog ->
            ( { model | dialogState = NoDialog }, Cmd.none )

        SetParamSearchInput val ->
            ( { model | paramSearchInputVal = val }, Cmd.none )

        ShowParamSearchDropdown ->
            ( { model | showParamSearchDropdown = not model.showParamSearchDropdown }, Cmd.none )

        SetDialogSearchInput val ->
            ( { model | dialogState = AddFilterDialog val }, Cmd.none )

        SetSearchFilterValue id val ->
            let
                searchStatus =
                    if val == "" || String.length val > 2 then
                        SearchPending
                    else
                        SearchNot

                newVal =
                    if val == "" then
                        NoValue
                    else
                        SearchValue val

                newFilters =
                    Search.updateFilterValue id newVal model.addedSampleFilters
            in
            ( { model | searchStatus = searchStatus, addedSampleFilters = newFilters }, Cmd.none )

        SetStringFilterValue id val selected ->
            let
                curVal =
                    Search.getFilterValue id (model.sampleFilters ++ model.addedSampleFilters ++ model.fileFilters)

                newVal =
                    case curVal of
                        NoValue ->
                            if selected then
                                SingleValue val
                            else
                                NoValue

                        SingleValue val2 ->
                            if selected then
                                MultipleValues [val2, val]
                            else
                                NoValue

                        MultipleValues vals ->
                            vals
                                |> Set.fromList
                                |> (if selected then Set.insert val else Set.remove val)
                                |> Set.toList
                                |> MultipleValues

                        _ -> -- error
                            NoValue

                sampleFilters =
                    Search.updateFilterValue id newVal model.sampleFilters

                addedSampleFilters =
                    Search.updateFilterValue id newVal model.addedSampleFilters

                fileFilters =
                    Search.updateFilterValue id newVal model.fileFilters

                dialogState =
                    case model.dialogState of
                        StringFilterDialog f ->
                            StringFilterDialog {f | value = newVal }

                        _ ->
                            model.dialogState

            in
            ( { model
                | searchStatus = SearchPending
                , sampleFilters = sampleFilters
                , addedSampleFilters = addedSampleFilters
                , fileFilters = fileFilters
                , dialogState = dialogState
              }
            , Cmd.none
            )

        SetFilterValue id val ->
            let
                sampleFilters =
                    Search.updateFilterValue id val model.sampleFilters

                addedSampleFilters =
                    Search.updateFilterValue id val model.addedSampleFilters

                fileFilters =
                    Search.updateFilterValue id val model.fileFilters

                cmd =
                    if id == purlLocation then
                        case val of
                            LatLngRadiusValue (lat,lng) radius ->
                                let
                                    lat2 =
                                        String.toFloat lat |> Maybe.withDefault 0

                                    lng2 =
                                        String.toFloat lng |> Maybe.withDefault 0

                                    radius2 =
                                        String.toFloat radius |> Maybe.withDefault 0
                                in
                                GMap.setLocation (Just (GMap.Location lat2 lng2 radius2))

                            _ ->
                                Cmd.none
                    else
                        Cmd.none
            in
            ( { model
                | searchStatus = SearchPending
                , sampleFilters = sampleFilters
                , addedSampleFilters = addedSampleFilters
                , fileFilters = fileFilters
              }
              , cmd
            )

        SetSearchTab label ->
            ( { model | searchTab = label }, Cmd.none )

        SetResultTab label ->
            ( { model | resultTab = label }, Cmd.none )

        SetSampleSortPos pos ->
            let
                direction =
                    if pos == model.sampleTableState.sortCol then
                        SortableTable.toggleDirection model.sampleTableState.sortDir
                    else
                        SortableTable.ASC

                newTableState =
                    SortableTable.State pos direction

            in
            ( { model | searchStatus = SearchPending, sampleTableState = newTableState }, Cmd.none )

        SetFileSortPos pos ->
            let
                direction =
                    if pos == model.fileTableState.sortCol then
                        SortableTable.toggleDirection model.fileTableState.sortDir
                    else
                        SortableTable.ASC

                newTableState =
                    SortableTable.State pos direction
            in
            ( { model | searchStatus = SearchPending, fileTableState = newTableState }, Cmd.none )

        SetPageSize size ->
            ( { model | searchStatus = SearchPending, pageSize = size }, Cmd.none )

        SetPageNum num ->
            let
                newPageNum =
                    if num >= 0 then
                        num
                    else
                        model.pageNum
            in
            if newPageNum /= model.pageNum then
                update (Search newPageNum False) model
            else
                ( model, Cmd.none )

        InputTimerTick _ ->
            case model.searchStatus of
                SearchPending ->
                    update (Search 0 False) model

                _ ->
                    ( model, Cmd.none )
            --TODO reset timer on new search trigger
            --if model.doSearch && Time.posixToMillis time - model.searchStartTime >= 1000 then -- 1 second
            --    update (Search 0 False) model
            --else
            --    ( model, Cmd.none )

        Search newPageNum download ->
            let
                allFilters =
                    (model.sampleFilters |> List.filter (\f -> f.value /= NoValue)) -- remove 4D/project if no value
                    ++ model.addedSampleFilters
                    ++ (model.fileFilters |> List.filter (\f -> f.value /= NoValue))
            in
            case generateQueryParams allFilters of
                Ok queryParams ->
                    let
                        fileSortCol =
                            model.fileTableState.sortCol * (SortableTable.directionToInt model.fileTableState.sortDir)

                        sampleSortCol =
                            model.sampleTableState.sortCol * (SortableTable.directionToInt model.sampleTableState.sortDir)

                        summaryParams =
                            ( "summary"
                            , (model.sampleFilters ++ model.addedSampleFilters)
                                |> List.filter (\f -> f.term.id /= purlLocation) -- summary not possible
                                |> List.filter (\f -> f.term.id /= purlProject) -- present by default
                                |> List.map (.term >> .id)
                                |> String.join ","
                            )

                        controlParams =
                            generateControlParams [] sampleSortCol fileSortCol model.pageSize (model.pageSize * newPageNum) model.showMap

                        allParams =
                            summaryParams ::
                            queryParams ++
                            controlParams
                    in
                    if download || allParams /= model.previousSearchParams then
                        ( { model
                            | searchStatus = SearchInProgress
                            , pageNum = newPageNum
                            , previousSearchParams = allParams
                          }
                        , if download then
                            Search.searchDownloadRequest allParams |> Http.send DownloadSearchCompleted
                          else
                            Search.searchRequest allParams |> Http.send SearchCompleted
                        )
                    else -- do nothing, search params haven't changed
                        ( { model | searchStatus = SearchNot }, Cmd.none )

                Err error ->
                    ( { model | searchStatus = SearchError error }, Cmd.none )

        SearchCompleted (Ok response) ->
            let
                displayedFilters =
                    (model.sampleFilters |> List.filter (\f -> f.value /= NoValue && validFilterValue f.value)) -- remove 4D/project if no value
                    ++ model.addedSampleFilters
            in
            ( { model
                --| sampleResultCount = response.count
                --, sampleResults = sampleResults
                --, summaryResults = Success response.summary
                --, mapResults = response.map
                | searchResponse = Success response
                , searchStatus = SearchNot
                , displayedSampleFilters = displayedFilters
              }
            , GMap.loadMap response.map
            )

        SearchCompleted (Err error) ->
            ( { model | searchStatus = SearchError (Error.toString error) }, Cmd.none )

        --FileSearchCompleted (Ok response) ->
        --    let
        --        fileResults =
        --            case response.results of
        --                FileSearchResults results ->
        --                    Success results
        --
        --                _ -> -- impossible
        --                    Failure "error"
        --    in
        --    ( { model
        --        | fileResultCount = response.count
        --        , fileResults = fileResults
        --        , mapResults = response.map
                --, searchStatus = SearchNot
              --}
            --, Cmd.none
            --)
        --
        --FileSearchCompleted (Err error) ->
        --    ( { model | searchStatus = SearchError (Error.toString error) }, Cmd.none ) --TODO

        DownloadSearchCompleted (Ok response) ->
            ( { model | searchStatus = SearchNot }
            , Download.string "PM_Search_Results.tsv" "text/tab-separated-values" response
            )

        DownloadSearchCompleted (Err error) ->
            ( { model | searchStatus = SearchError (Error.toString error) }, Cmd.none )

        ToggleMap ->
            ( { model | showMap = not model.showMap }, Cmd.none )

        UpdateLocationFromMap maybeLoc ->
            let
                newLocationVal =
                    case maybeLoc of
                        Just loc ->
                            LatLngRadiusValue (String.fromFloat loc.lat, String.fromFloat loc.lng) (String.fromFloat loc.radius)

                        Nothing ->
                            NoValue

                locationFilter =
                    Filter { defaultSearchTerm | id = purlLocation } newLocationVal

                newFilters =
                    Search.updateFilter purlLocation locationFilter model.sampleFilters
            in
            ( { model | searchStatus = SearchPending, sampleFilters = newFilters }, Cmd.none )

        MapLoaded _ ->
            ( { model | mapLoaded = True }, Cmd.none )

        InitDatePickers date ->
            let
                dateRangePicker =
                    { start = DatePicker.initFromDate date
                    , end = DatePicker.initFromDate date
                    }
            in
            ( { model | dateRangePickers = Dict.insert purlDateTimeISO dateRangePicker Dict.empty }
            , Cmd.none
            )

        SetStartDatePicker id val subMsg -> --TODO merge with SetEndDatePicker
            let
                dateRangePicker =
                    Dict.get id model.dateRangePickers
            in
            case dateRangePicker of
                Just picker ->
                    let
                        ( newDatePicker, dateEvent ) =
                            DatePicker.update startDatePickerSettings subMsg picker.start

                        newDateRangePicker =
                            { start = newDatePicker
                            , end = picker.end
                            }

                        newDate =
                            case dateEvent of
                                Picked date ->
                                    Just date

                                _ ->
                                    Nothing

                        newDateVal =
                            case newDate of
                                Just date ->
                                    case val of
                                        DateTimeValue _ ->
                                            DateTimeValue (Date.toIsoString date)

                                        DateTimeRangeValue _ dt2 ->
                                            DateTimeRangeValue (Date.toIsoString date) dt2

                                        _ ->
                                            DateTimeRangeValue (Date.toIsoString date) ""

                                Nothing ->
                                    val

                        newFilters =
                            Search.updateFilterValue id newDateVal model.sampleFilters
                    in
                    ({ model
                        | searchStatus = if newDate /= Nothing then SearchPending else SearchNot
                        , sampleFilters = newFilters
                        , dateRangePickers = Dict.insert id newDateRangePicker model.dateRangePickers
                    }
                    , Cmd.none
                    )

                Nothing -> -- should never happen
                    ( model, Cmd.none )

        SetEndDatePicker id val subMsg ->
            let
                dateRangePicker =
                    Dict.get id model.dateRangePickers
            in
            case dateRangePicker of
                Just picker ->
                    let
                        ( newDatePicker, dateEvent ) =
                            DatePicker.update endDatePickerSettings subMsg picker.end

                        newDateRangePicker =
                            { start = picker.start
                            , end = newDatePicker
                            }

                        newDate =
                            case dateEvent of
                                Picked date ->
                                    Just date

                                _ ->
                                    Nothing

                        newDateVal =
                            case newDate of
                                Just date ->
                                    case val of
                                        DateTimeValue _ ->
                                            DateTimeValue (Date.toIsoString date)

                                        DateTimeRangeValue dt1 _ ->
                                            DateTimeRangeValue dt1 (Date.toIsoString date)

                                        _ ->
                                            DateTimeRangeValue "" (Date.toIsoString date)

                                Nothing ->
                                    val

                        newFilters =
                            Search.updateFilterValue id newDateVal model.sampleFilters
                    in
                    ({ model
                        | searchStatus = if newDate /= Nothing then SearchPending else SearchNot
                        , sampleFilters = newFilters
                        , dateRangePickers = Dict.insert id newDateRangePicker model.dateRangePickers
                    }
                    , Cmd.none
                    )

                Nothing -> -- should never happen
                    ( model, Cmd.none )

        CartMsg subMsg ->
            let
                cart =
                    Session.getCart model.session

                updateCart =
                    case subMsg of
                        Cart.AddToCart idList ->
                            (Cart.size cart) + (List.length idList) <= maxCartSize

                        _ ->
                            True
            in
            if updateCart then
                let
                    newCart =
                        Cart.update subMsg cart

                    newSession =
                        Session.setCart model.session newCart
                in
                ( { model | session = newSession }
                , Cmd.batch
                    [ --Cmd.map CartMsg subCmd
                    Cart.store newCart
                    ]
                )
            else
                let
                    messageDialog =
                        MessageDialog ("Too many files to add to the cart (>" ++ (String.fromInt maxCartSize) ++ "). Try further constraining the search parameters.")
                in
                ( { model | dialogState = messageDialog }, Cmd.none )


--TODO finish validation and error reporting
generateQueryParams : List Filter -> Result String (List (String, String))
generateQueryParams filters =
        let
            termParams =
                filters
                    |> List.filter (.value >> validFilterValue)
                    |> List.map (\f ->
                        if f.term.id == purlDateTimeISO then
                            datetimeParam f
                        else
                            [ Tuple.pair f.term.id ( Search.filterValueToString f.value ) ]
                       )
                    |> List.concat

            datetimeParam f =
                if validFilterValue f.value then
                    let
                        encodedVal =
                            Search.filterValueToString f.value
                    in
                    [ ( "|" ++ purlDateTimeISO, encodedVal )
                    , ( "|" ++ purlDateTimeISOStart, encodedVal )
                    , ( "|" ++ purlDateTimeISOEnd, encodedVal )
                    ]
                else
                    []
        in
        Ok termParams


generateControlParams : List String -> Int -> Int -> Int -> Int -> Bool -> List (String, String)
generateControlParams columns sampleSortCol fileSortCol limit offset showMap =
    [ ("columns", String.join "," columns)
    , ("sampleSort", String.fromInt sampleSortCol)
    , ("fileSort", String.fromInt fileSortCol)
    , ("limit", String.fromInt limit)
    , ("offset", String.fromInt offset)
    , ("map", if showMap then "1" else "0")
    ]



-- VIEW --


view : Model -> Html Msg
view model =
    case model.searchStatus of
        SearchInit _ ->
            Page.viewSpinnerOverlay

        _ ->
            div [ class "container-fluid" ]
                [ br [] []
                , div [ class "float-left", style "width" "26%" ]
                    [ viewSearchPanel model ]
                , div [ class "float-right", style "width" "74%", style "padding-left" "1em" ]
                    [ viewMap model.showMap model.mapLoaded
                    , viewResults model
                    ]
                , viewDialogs model
                ]


viewDialogs : Model -> Html Msg
viewDialogs model =
    case model.dialogState of
        NoDialog ->
            viewBlank

        AddFilterDialog searchStr ->
            viewAddFilterDialog model.allTerms searchStr

        StringFilterDialog filter ->
            viewStringFilterDialog filter

        FilterSummaryDialog term ->
            viewSearchTermSummaryDialog term

        MessageDialog msg ->
            Page.viewMessageDialog msg CloseDialog


viewSearchPanel : Model -> Html Msg
viewSearchPanel model =
    div [ style "border" "1px solid lightgray", style "width" "25.5vw" ]
        [ ul [ class "nav nav-tabs" ]
            ( (List.map (\lbl -> viewTab lbl (lbl == model.searchTab) SetSearchTab) [ "Samples", "Files" ]) ++
              [ (li [ class "nav-item ml-auto" ]
                [ a [ class "small nav-link", href "", style "font-weight" "bold", onClick ClearFilters ]
                    [ text "Reset" ]
                ])
              ]
            )
        , if model.searchTab == "Samples" then
            viewSampleSearchPanel model
          else if model.searchTab == "Files" then
            viewFileSearchPanel model.fileFilters
          else
            text ""
        ]


viewSampleSearchPanel : Model -> Html Msg
viewSampleSearchPanel model =
    let
        projectFilter =
            model.sampleFilters
                |> List.filter (\f -> f.term.id == purlProject)
                |> List.head
                |> Maybe.withDefault (Search.defaultFilter purlProject "Project")
    in
    div []
        [ view4DPanel model
        , viewStringFilterPanel projectFilter
        , viewAddedFiltersPanel model.addedSampleFilters model.dateRangePickers
        , viewAddFilterPanel model.showParamSearchDropdown model.paramSearchInputVal model.allTerms model.addedSampleFilters
        ]


viewFileSearchPanel : List Filter -> Html Msg
viewFileSearchPanel fileFilters =
    div []
        (List.map viewStringFilterPanel fileFilters)


viewTab : String -> Bool -> (String -> Msg) -> Html Msg
viewTab label isSelected msg =
    li [ class "nav-item" ]
        [ a [ class "nav-link", classList [ ("active", isSelected), ("font-weight-bold", isSelected) ], href "", style "color" "black", onClick (msg label) ]
            [ text label ]
        ]


view4DPanel : Model -> Html Msg
view4DPanel model =
    let
        locationVal =
            Search.getFilterValue purlLocation model.sampleFilters

        depthVal =
            Search.getFilterValue purlDepth model.sampleFilters

        datetimeVal =
            Search.getFilterValue purlDateTimeISO model.sampleFilters

        dateRangePicker =
            Dict.get purlDateTimeISO model.dateRangePickers
    in
    div []
        [ div [ class "card", style "font-size" "0.85em" ]
            [ div [ class "card-body" ]
                [ h6 [ style "color" "darkblue"]
                    [ text (String.fromChar (Char.fromCode 9660))
                    , text " Time/Space"
                    , small [] [ a [ class "alert-link float-right", href "", onClick ToggleMap ]
                        [ if model.showMap then text "Close Map" else text "View Map" ] ]
                    ]
                , Html.form [ style "padding-top" "0.5em" ]
                    [ div [ class "form-row" ]
                        [ div [ class "input-group input-group-sm" ]
                            ((div [ class "input-group-prepend" ] [ span [ class "input-group-text", style "width" "5em" ] [ text "Location"] ])
                                :: (viewLocationFilterInput locationVal)
                                ++ [ div [ class "input-group-append" ]
                                    [ viewFormatButton
                                    , div [ class "dropdown-menu" ]
                                        [ a [ class "dropdown-item active", href "", onClick (SetFilterValue purlLocation (LatLngRadiusValue ("","") "")) ]
                                            [ text "Lat, Lng (deg), Radius (km)" ]
                                        , a [ class "dropdown-item disabled", href "", disabled True, onClick (SetFilterValue purlLocation (LonghurstValue "")) ]
                                            [ text "Longhurst Province - coming soon" ]
                                        ]
                                    ]
                                ]
                            )
                        ]
                    , br [] []
                    , div [ class "form-row" ]
                        [ div [ class "input-group input-group-sm" ]
                            ((div [ class "input-group-prepend" ] [ span [ class "input-group-text", style "width" "5em" ] [ text "Depth"] ])
                                :: (viewNumberFilterInput purlDepth depthVal)
                                ++ [ viewNumberFilterFormatOptions purlDepth depthVal ]
                            )
                        ]
                    , br [] []
                    , div [ class "form-row" ]
                        [ div [ class "input-group input-group-sm" ]
                            ((div [ class "input-group-prepend" ] [ span [ class "input-group-text", style "width" "5em" ] [ text "Date"] ]) ::
                                (case dateRangePicker of
                                    Just picker ->
                                        (viewDateTimeFilterInput picker purlDateTimeISO datetimeVal ++
                                            [ viewDateTimeFilterFormatOptions purlDateTimeISO datetimeVal ]
                                        )

                                    _ -> -- error, shouldn't happen
                                        [ text "Error" ]
                                )
                            )
                        ]
                    , br [] []
                    ]
                ]
            ]
        ]


viewAddFilterPanel : Bool -> String -> List SearchTerm -> List Filter -> Html Msg
viewAddFilterPanel showDropdown searchVal allTerms filters =
    let
        filterIDs =
            List.map (.term >> .id) filters

        makeOption term =
            let
                dis =
                    List.member term.id filterIDs
            in
            a [ classList [ ("dropdown-item", True), ("disabled", dis) ], href "", onClick (AddFilter term.id) ]
                [ text <| String.Extra.toSentenceCase term.label ]

        isRedundant term =
            List.member term.id redundantTerms

        filterOnSearch term =
            let
                lowerSearchVal =
                    String.toLower searchVal
            in
            String.contains lowerSearchVal (String.toLower term.label)
                || String.contains lowerSearchVal (String.toLower term.id)

        options =
            allTerms
                |> List.filter (not << isRedundant)
                |> List.filter filterOnSearch
                |> List.sortWith (\a b -> compare (String.Extra.toSentenceCase a.label) (String.Extra.toSentenceCase b.label) )
                |> List.map makeOption

        show =
            searchVal /= "" || showDropdown
    in
    viewPanel "" "Add Filter" "" "" Nothing Nothing (Just "#d8edf3")
        [ div [ class "input-group input-group-sm", style "position" "relative" ]
            [ input [ type_ "text", class "form-control", placeholder "Search parameters", value searchVal, onInput SetParamSearchInput ] []
            , div [ class "input-group-append" ]
                [ button [ class "btn btn-outline-secondary", type_ "button", onClick (OpenDialog (AddFilterDialog "")) ] [ text "?" ]
                , button [ class "btn btn-outline-secondary dropdown-toggle", type_ "button", onClick ShowParamSearchDropdown ] []
                , div [ class "dropdown-menu", classList [("show", show)], style "position" "absolute", style "left" "0px", style "max-height" "30vh", style "overflow-y" "auto" ] options
                ]
            ]
        ]


viewAddFilterDialog : List SearchTerm -> String -> Html Msg
viewAddFilterDialog allTerms searchVal =
    let
        isRedundant term =
            List.member term.id redundantTerms

        filterOnSearch term =
            String.contains (String.toLower searchVal) (String.toLower term.label)
                || String.contains (String.toLower searchVal) (String.toLower term.id)

        terms =
            allTerms
                |> List.filter (not << isRedundant)
                |> List.filter filterOnSearch
                |> List.sortWith (\a b -> compare (String.Extra.toSentenceCase a.label) (String.Extra.toSentenceCase b.label) )

        count =
            List.length terms

        noneIfBlank s = --TODO move into Util module
            if s == "" then
                "None"
            else
                s

        viewTerm term =
            div [ class "border-bottom px-2" ]
                [ table [ style "width" "97%" ]
                    [ tr []
                        [ td [] [ a [ href term.id, target "_blank" ] [ text (String.Extra.toSentenceCase term.label) ] ]
                        , td [] [ button [ class "btn btn-light btn-sm border float-right", onClick (AddFilter term.id) ] [ text "Add" ] ]
                        ]
                    ]
                , (if term.definition /= "" then
                    table [ class "table table-borderless table-sm small"]
                        [ tr []
                            [ th [] [ text "Definition"]
                            , td [] [ text term.definition ]
                            ]
                        , tr []
                            [ th [] [ text "Unit"]
                            , td [] [ a [ href term.unitId, target "_blank" ] [ text (noneIfBlank term.unitLabel) ] ]
                            ]
                        ]
                  else
                    text "")
                ]
    in
    viewDialog "Add Filter"
        [ input [ type_ "text", class "form-control", placeholder "Search parameters", onInput SetDialogSearchInput ] []
        , div [ class "small text-secondary float-right" ]
            [ if count == 0 then
                text "No results"
              else
                text <| (String.fromInt count) ++ " result" ++ (if count == 1 then "" else "s")
            ]
        , div [ class "mt-5 border-top", style "overflow-y" "auto", style "max-height" "50vh" ]
            (List.map viewTerm terms)
        ]
        [ button [ type_ "button", class "btn btn-secondary", onClick CloseDialog ] [ text "Close" ] ]
        CloseDialog


viewAddedFiltersPanel : List Filter -> Dict PURL DateRangePicker -> Html Msg
viewAddedFiltersPanel filters dateRangePickers =
    div []
        (filters
            |> List.map
                (\f ->
                    case f.term.type_ of
                        "string" ->
                            if List.length f.term.distribution >= minNumPanelOptionsForSearchBar then
                                viewSearchFilterPanel f
                            else
                                viewStringFilterPanel f

                        "number" ->
                            viewNumberFilterPanel f

                        "datetime" ->
                            let
                                dateRangePicker =
                                    Dict.get f.term.id dateRangePickers
                            in
                            case dateRangePicker of
                                Just picker ->
                                    viewDateTimeFilterPanel picker f

                                _ ->
                                    text "Error"

                        _ ->
                            text "Error"
                )
        )


viewSearchFilterPanel : Filter -> Html Msg
viewSearchFilterPanel filter =
    let
        numValues =
            List.length filter.term.distribution

        val2 =
            case filter.value of
                SearchValue s ->
                    s

                _ ->
                    ""
    in
    viewTermPanel filter.term
        [ label [] [ numValues |> toFloat |> format { usLocale | decimals = 0 } |> text, text " unique values" ]
        , div [ class "input-group input-group-sm" ]
            [ input [ type_ "text", class "form-control", placeholder "Search ...", value val2, onInput (SetSearchFilterValue filter.term.id) ] [] ]
        ]


viewStringFilterPanel : Filter -> Html Msg
viewStringFilterPanel filter =
    let
        numOptions =
            List.length filter.term.distribution
    in
    viewTermPanel filter.term
        [ div []
            (viewStringFilterOptions filter maxNumPanelOptions True)
        , if numOptions > maxNumPanelOptions then
            button [ class "btn btn-sm btn-link float-right", onClick (OpenDialog (StringFilterDialog filter)) ]
                [ text <| String.fromInt (numOptions - maxNumPanelOptions) ++ " More ..." ]
          else
            viewBlank
        ]


viewStringFilterDialog : Filter -> Html Msg
viewStringFilterDialog filter =
    viewDialog (String.Extra.toTitleCase filter.term.label)
        [ div [ style "overflow-y" "auto", style "max-height" "50vh" ] (viewStringFilterOptions filter 9999 False) ]
        [ button [ type_ "button", class "btn btn-secondary", onClick CloseDialog ] [ text "Close" ] ]
        CloseDialog


viewStringFilterOptions : Filter -> Int -> Bool -> List (Html Msg)
viewStringFilterOptions filter maxNum sortSelected =
    let
        sortByCount a b =
            case compare (Tuple.second a) (Tuple.second b) of
                LT -> GT
                EQ -> EQ
                GT -> LT

        sortBySelected a b =
            case ( Search.isStringFilterSelected (Tuple.first a) filter.value, Search.isStringFilterSelected (Tuple.first b) filter.value ) of
                (True, False) ->
                    LT

                (False, True) ->
                    GT

                (_, _) ->
                    sortByCount a b

        sortByName a b =
            compare (Tuple.first a |> purlToLabel filter.term) (Tuple.first b |> purlToLabel filter.term)

        numSelected =
            filter.term.distribution
                |> List.filter (\a -> Search.isStringFilterSelected (Tuple.first a) filter.value)
                |> List.length

        truncatedOptions =
            filter.term.distribution
                |> List.sortWith
                    (if sortSelected then
                        sortBySelected
                    else
                        sortByName
                    )
                |> List.take (Basics.max maxNum numSelected)

        viewRow (name, count) =
            -- Using table layout to fix issue with wrapping rows
            table [ style "width" "100%" ]
                [ tr []
                    [ td []
                        [ div [ class "form-check form-check-inline" ]
                            [ input [ class "form-check-input", type_ "checkbox", checked (Search.isStringFilterSelected name filter.value), onCheck (SetStringFilterValue filter.term.id name) ] []
                            , label [ class "form-check-label" ] [ name |> purlToLabel filter.term |> String.Extra.toSentenceCase |> text ]
                            ]
                        ]
                    , td [ style "max-width" "3em" ]
                        [ div [ class "badge badge-secondary float-right align-top" ]
                            [ count |> toFloat |> format { usLocale | decimals = 0 } |> text ]
                        ]
                    ]
                ]
    in
    List.map viewRow truncatedOptions


viewNumberFilterPanel : Filter -> Html Msg
viewNumberFilterPanel filter =
    viewTermPanel filter.term
        [ div [ class "input-group input-group-sm" ]
            (List.append (viewNumberFilterInput filter.term.id filter.value)
                [ viewNumberFilterFormatOptions filter.term.id filter.value
                ]
            )
        ]


viewNumberFilterInput : PURL -> FilterValue -> List (Html Msg)
viewNumberFilterInput id val =
    let
        rangeInput min max =
            [ input [ type_ "text", class "form-control", placeholder "min", value min, onInput (\p -> SetFilterValue id (RangeValue p max)) ] []
            , input [ type_ "text", class "form-control", placeholder "max", value max, onInput (\p -> SetFilterValue id (RangeValue min p)) ] []
            ]
    in
    case val of
        SingleValue s ->
            [ input [ type_ "text", class "form-control", placeholder "value", value s, onInput (\p -> SetFilterValue id (SingleValue p)) ] []
            ]

        RangeValue min max ->
            rangeInput min max

        OffsetValue n offset ->
            [ input [ type_ "text", class "form-control", placeholder "value", value n, onInput (\p -> SetFilterValue id (OffsetValue p offset)) ] []
            , input [ type_ "text", class "form-control", placeholder "+/-", value offset, onInput (\p -> SetFilterValue id (OffsetValue n p)) ] []
            ]

        _ ->
            rangeInput "" ""


viewNumberFilterFormatOptions : PURL -> FilterValue -> Html Msg
viewNumberFilterFormatOptions id val =
    let
        viewOption (label, filterVal) =
            let
                isSelected =
                    case (val, filterVal) of --FIXME kludgey
                        (SingleValue _, SingleValue _) ->
                            True

                        (NoValue, RangeValue _ _) -> -- this is the default
                            True

                        (RangeValue _ _, RangeValue _ _) ->
                            True

                        (OffsetValue _ _, OffsetValue _ _) ->
                            True

                        (_, _) ->
                            False
            in
            a [ class "dropdown-item", classList [ ("active", isSelected) ], href "", onClick (SetFilterValue id filterVal) ]
                [ text <| String.Extra.toSentenceCase label ]

        options =
            [ ("exact", SingleValue ""), ("range", RangeValue "" ""), ("offset", OffsetValue "" "") ]
    in
    div [ class "input-group-append" ]
        [ viewFormatButton
        , div [ class "dropdown-menu" ]
            (List.map viewOption options)
        ]


viewDateTimeFilterPanel : DateRangePicker -> Filter -> Html Msg
viewDateTimeFilterPanel dateRangePicker filter =
    viewTermPanel filter.term
        [ div [ class "input-group input-group-sm" ]
            (List.append (viewDateTimeFilterInput dateRangePicker filter.term.id filter.value)
                [ viewDateTimeFilterFormatOptions filter.term.id filter.value
                ]
            )
        ]


viewDateTimeFilterInput : DateRangePicker -> PURL -> FilterValue -> List (Html Msg)
viewDateTimeFilterInput dateRangePicker id val =
    let
        singleInput dt =
            let
                date =
                    Date.fromIsoString dt |> Result.toMaybe
            in
            [ DatePicker.view
                date
                defaultDatePickerSettings
                dateRangePicker.start
                |> Html.map (SetStartDatePicker id val)
            ]

        rangeInput dt1 dt2 =
            let
                date1 =
                    Date.fromIsoString dt1 |> Result.toMaybe

                date2 =
                    Date.fromIsoString dt2 |> Result.toMaybe
            in
            [ DatePicker.view
                date1
                startDatePickerSettings
                dateRangePicker.start
                |> Html.map (SetStartDatePicker id val)
            , DatePicker.view
                date2
                endDatePickerSettings
                dateRangePicker.end
                |> Html.map (SetEndDatePicker id val)
            ]
    in
    case val of
        DateTimeValue dt ->
            singleInput dt

        DateTimeRangeValue dt1 dt2 ->
            rangeInput dt1 dt2

        _ ->
            rangeInput "" ""


defaultDatePickerSettings =
    let
        defaultSettings =
            DatePicker.defaultSettings
    in
    { defaultSettings
        | placeholder = "value"
        , dateFormatter = Date.format "yyyy-MM-dd"
        , containerClassList = [ ( "input-group-prepend", True ) ]
    }


startDatePickerSettings =
    { defaultDatePickerSettings | placeholder = "start" }


endDatePickerSettings =
    { defaultDatePickerSettings | placeholder = "end" }


viewDateTimeFilterFormatOptions : PURL -> FilterValue -> Html Msg
viewDateTimeFilterFormatOptions id val =
    let
        viewOption (label, filterVal) =
            let
                isSelected =
                    case (val, filterVal) of --FIXME kludgey
                        (DateTimeValue _, DateTimeValue _) ->
                            True

                        (DateTimeRangeValue _ _, DateTimeRangeValue _ _) ->
                            True

                        _ ->
                            False
            in
            a [ class "dropdown-item", classList [ ( "active", isSelected ) ], href "", onClick (SetFilterValue id filterVal) ]
                [ text <| String.Extra.toSentenceCase label ]

        options =
            [ ("Exact (YYYY-MM-DD)", DateTimeValue "")
            , ("Range (YYYY-MM-DD to YYYY-MM-DD)", DateTimeRangeValue "" "")
            ]
    in
    div [ class "input-group-append" ]
        [ viewFormatButton
        , div [ class "dropdown-menu" ]
            (List.map viewOption options)
        ]


viewLocationFilterInput : FilterValue -> List (Html Msg)
viewLocationFilterInput val =
    let
        setFilterValue lat lng radius =
            SetFilterValue purlLocation (LatLngRadiusValue (lat,lng) radius)

        latLngRadiusInput lat lng radius =
            [ input [ type_ "text", class "form-control", placeholder "lat", value lat, onInput (\p -> setFilterValue p lng radius) ] []
            , input [ type_ "text", class "form-control", placeholder "lng", value lng, onInput (\p -> setFilterValue lat p radius) ] []
            , input [ type_ "text", class "form-control", placeholder "radius", value radius, onInput (\p -> setFilterValue lat lng p) ] []
            ]
    in
    case val of
        LatLngRadiusValue (lat,lng) radius ->
            latLngRadiusInput lat lng radius

        LonghurstValue s ->
            [ input [ type_ "text", class "form-control", placeholder "Longhurst province", value s, onInput (\p -> SetFilterValue purlLocation (LonghurstValue p)) ] [] ]

        _ ->
            latLngRadiusInput "" "" ""


viewTermPanel : SearchTerm -> List (Html Msg) -> Html Msg
viewTermPanel term nodes =
    let
        removeMsg =
            if (List.map (.term >> .id) permanentSampleFilters) ++ permanentFileTerms |> List.member term.id then
                Nothing
            else
                Just RemoveFilter
    in
    viewPanel term.id term.label term.unitId term.unitLabel removeMsg (Just (OpenDialog (FilterSummaryDialog term))) Nothing nodes


--TODO move configuration params into type alias (like "type alias PanelConfig = {}")
viewPanel : PURL -> String -> PURL -> String -> Maybe (PURL -> Msg) -> Maybe Msg -> Maybe String -> List (Html Msg) -> Html Msg
viewPanel id title unitId unitLabel maybeRemoveMsg maybeOpenChartMsg maybeBgColor nodes =
    let
        header =
            h6 [ style "color" "darkblue"]
                [ text (String.fromChar (Char.fromCode 9660))
                , text " "
                , if String.startsWith "http" id then
                    a [ href id, target "_blank" ]
                        [ text (String.Extra.toTitleCase title) ]
                  else
                    text (String.Extra.toTitleCase title)
                , text " "
                , if unitLabel /= "" then
                    small [ style "margin-left" "5px" ]
                        [ a [ href unitId, target "_blank" ]
                            [ text ("[" ++ unitLabel ++ "]") ]
                        ]
                  else
                    viewBlank
                , case maybeRemoveMsg of
                    Just removeMsg ->
                        span [ class "float-right ml-2", style "cursor" "pointer", onClick (removeMsg id) ]
                            [ text (String.fromChar (Char.fromCode 10005)) ]

                    Nothing ->
                        viewBlank
                , case maybeOpenChartMsg of
                    Just openMsg ->
                        span [ class "float-right", style "cursor" "pointer", onClick openMsg ]
                            [ Icon.barChart ]

                    Nothing ->
                        viewBlank
                ]
    in
    div [ class "card", style "font-size" "0.85em", style "background" (maybeBgColor |> Maybe.withDefault "") ]
        [ div [ class "card-body" ]
            (header :: nodes)
        ]


viewResults : Model -> Html Msg
viewResults model =
    let
        ( content, maybeCount ) =
            case model.searchResponse of
                Success response ->
                    if model.resultTab == "Files" then
                        ( viewFileResults model, Just response.fileCount )
                    else if model.resultTab == "Samples" then
                        ( viewSampleResults model, Just response.sampleCount )
                    else -- "Summary"
                        ( viewSummary model, Just response.sampleCount )

                Failure msg ->
                    ( viewError msg, Nothing )

                _ ->
                    ( viewBlank, Nothing )
    in
    div [ style "min-height" "50em" ]
        [ case model.searchStatus of
            SearchInit _ ->
                Page.viewSpinnerOverlay

            SearchError msg ->
                viewError msg

            _ ->
                div []
                    [ if model.searchStatus == SearchPending || model.searchStatus == SearchInProgress then
                        Page.viewSpinnerOverlay
                      else
                        viewBlank
                    , if maybeCount == Nothing then
                        viewBlank
                      else if maybeCount == Just 0 then
                        h1 [ class "text-center mt-5", style "min-height" "5.5em" ] [ text "No results" ]
                      else
                        div [ style "border" "1px solid lightgray" ]
                            [ ul [ class "nav nav-tabs", style "width" "100%" ]
                                ((List.map (\lbl -> viewTab lbl (lbl == model.resultTab) SetResultTab) [ "Summary", "Samples", "Files" ] )
                                 --++ [ li [ class "nav-item ml-auto" ]
                                 --       [ a [ class "small nav-link", href "", style "font-weight" "bold" ] [ text "Columns" ] ]
                                 --   ]
                                 ++ [ li [ class "nav-item ml-auto" ]
                                        [ a [ class "nav-link", href "", onClick (Search 0 True) ] [ Icon.fileDownload, text " Download" ] ]
                                    --, li [ class "nav-item" ]
                                    --    [ a [ class "nav-link", href "" ] [ Icon.cloudDownload ] ]
                                    ]
                                )
                            ,
                            div []
                                [ if model.resultTab /= "Summary" then
                                    viewPageSummary model.pageNum model.pageSize (maybeCount |> Maybe.withDefault 0) model.resultTab
                                  else
                                    viewBlank
                                , content
                                , if model.resultTab /= "Summary" then
                                    viewPageControls model.pageNum model.pageSize (maybeCount |> Maybe.withDefault 0)
                                  else
                                    viewBlank
                                ]
                            ]
                    ]
        ]


viewError : String -> Html Msg
viewError msg =
    div [ class "alert alert-danger m-3" ]
        [ p [] [ text "An error occurred:" ]
        , p [] [ text msg ]
        ]


viewPageSummary : Int -> Int -> Int -> String -> Html Msg
viewPageSummary curPageNum pageSize resultCount label =
    div [ class "small ml-1", style "color" "dimgray" ]
        [ text "Showing "
        , curPageNum * pageSize + 1 |> Basics.max 1 |> String.fromInt |> text
        , text " - "
        , curPageNum * pageSize + pageSize |> Basics.max 1 |> Basics.min resultCount |> String.fromInt |> text
        , text " of "
        , resultCount |> toFloat |> format { usLocale | decimals = 0 } |> text
        , text " "
        , text label
        --, (if resultCount /= 1 then "s" else "") |> text -- pluralize
        ]


viewPageControls : Int -> Int -> Int -> Html Msg
viewPageControls curPageNum pageSize resultCount =
    let
        sizeOption size =
            a [ class "dropdown-item", href "", onClick (SetPageSize size) ] [ text (String.fromInt size) ]

        pageOption label num =
            let
                dis =
                    num < 0 -- previous
                        || num == curPageNum -- current
                        || num > lastPageNum -- next
            in
            li [ classList [ ("page-item", True), ("disabled", dis) ] ]
                [ a [ class "page-link", href "", onClick (SetPageNum num) ] [ text label ] ]

        lastPageNum =
            (toFloat resultCount) / (toFloat pageSize) |> floor
    in
    div []
        [ div [ class "float-left" ]
            [ text "Show "
            , div [ class "dropup", style "display" "inline" ]
                [ button [ class "btn btn-secondary dropdown-toggle", type_ "button", attribute "data-toggle" "dropdown" ] [ text (String.fromInt pageSize) ]
                , div [ class "dropdown-menu" ]
                    (List.map sizeOption [20, 40, 60, 80, 100])
                ]
            , text " results"
        ]
        , nav [ class "float-right" ]
            [ ul [ class "pagination" ]
                --FIXME refactor
                [ pageOption "First" 0
                , pageOption "Previous" (curPageNum - 1)
                , pageOption (String.fromInt (curPageNum + 1)) curPageNum
                , if curPageNum + 1 > lastPageNum then text "" else pageOption (String.fromInt (curPageNum + 2)) (curPageNum + 1)
                , if curPageNum + 2 > lastPageNum then text "" else pageOption (String.fromInt (curPageNum + 3)) (curPageNum + 2)
                , if curPageNum + 3 > lastPageNum then text "" else pageOption "..." (curPageNum + 3)
                , pageOption "Next" (curPageNum + 1)
                , pageOption "Last" lastPageNum
                ]
            ]
        ]


viewSummary : Model -> Html Msg
viewSummary model =
    case model.searchResponse |> RemoteData.map .summary of
        Success results ->
            let
                projectData =
                    List.head results |> Maybe.withDefault []

                termResults =
                    List.tail results |> Maybe.withDefault []

                termLabels =
                    (model.sampleFilters ++ model.addedSampleFilters)
                        |> List.filter (\f -> f.term.id /= purlLocation)
                        |> List.filter (\f -> f.term.id /= purlProject)
                        |> List.map (.term >> .label)
            in
            div [ style "margin" "1em" ]
                (viewSearchTermSummaryChart "project" projectData ::
                    (List.Extra.zip termLabels termResults
                        |> List.map (\(label, data) -> viewSearchTermSummaryChart label data)
                    )
                )

        _ ->
            viewBlank


viewSearchTermSummaryChart : String -> List (String, Int) -> Html Msg
viewSearchTermSummaryChart label data =
    let
        defaultBarChartConfig =
            BarChart.defaultConfig

        maxTitleLen =
            50

        title =
            "# Samples by " ++ (String.Extra.toTitleCase label)

        truncTitle =
            if String.length title < maxTitleLen then
                title
            else
                (String.slice 0 maxTitleLen title) ++ "... "

        config =
            { defaultBarChartConfig
                | title = Just truncTitle
                , width = 400
                , height = 400
            }
    in
    BarChart.view config
        (data
            |> List.map (Tuple.mapSecond toFloat)
        )


viewSearchTermSummaryDialog : SearchTerm -> Html Msg
viewSearchTermSummaryDialog term =
    let
        newDist =
            term.distribution |> List.map (Tuple.mapFirst (purlToLabel term))

        sortedDist =
            if term.type_ /= "number" then
                List.sortBy Tuple.first newDist
            else
                newDist
    in
    viewDialog (String.Extra.toTitleCase term.label)
        [ div [ style "overflow-y" "auto", style "max-height" "50vh", style "text-align" "center", style "margin-top" "2em" ]
            [ viewSearchTermSummaryChart term.label sortedDist ]
        ]
        [ button [ type_ "button", class "btn btn-secondary", onClick CloseDialog ]
            [ text "Close" ]
        ]
        CloseDialog


viewSampleResults : Model -> Html Msg
viewSampleResults model =
    let
        colHeaders =
            "Project Name" ::
            "Sample ID" ::
            (model.displayedSampleFilters
                |> List.filter (\f -> f.term.id /= "project")
                |> List.map
                    (\f ->
                        if f.term.id == purlLocation && f.value /= NoValue && validFilterValue f.value then
                            [ "Location" ]
                        else if f.term.id == purlDepth && f.value /= NoValue && validFilterValue f.value then
                            [ "Depth" ]
                        else if f.term.id == purlDateTimeISO && f.value /= NoValue && validFilterValue f.value then
                            [ "Date", "Start Date", "End Date" ]
                        else if f.term.unitLabel /= "" then
                            [ f.term.label ++ " (" ++ f.term.unitLabel ++ ")" ]
                        else
                            [ f.term.label ]
                    )
                |> List.concat
            )

        addToCartTh =
            th [ class "text-right", style "min-width" "10em" ]
                [ Cart.addAllToCartButton (Session.getCart model.session) Nothing
                    (model.searchResponse
                        |> RemoteData.map .fileIDs
                        |> RemoteData.toMaybe
                        |> Maybe.withDefault []
                    )
                    |> Html.map CartMsg
                ]

        columns =
            [ tr [] (List.indexedMap mkTh colHeaders ++ [ addToCartTh ]) ]

        mkTh index label =
            let
                pos =
                    index + 1

                lbl =
                    String.Extra.toTitleCase
                        (if pos == abs model.sampleTableState.sortCol then
                            label ++ " " ++ (viewUpDownArrow model.sampleTableState.sortDir)
                        else
                            label
                        )
            in
            th [ style "cursor" "pointer", style "max-width" maxResultColWidth, onClick (SetSampleSortPos pos) ] [ text lbl ]

        mkTd label =
            td [ style "max-width" maxResultColWidth ] [ text label ]

        mkTr result =
            tr []
                ([ td [] [ a [ Route.href (Route.Project result.projectId) ] [ text result.projectName ] ]
                , td [] [ a [ Route.href (Route.Sample result.sampleId) ] [ text result.sampleAccn ] ]
                ] ++
                (result.values
                    |> List.map
                        (\vals ->
                            Search.searchResultValuesToString vals |> mkTd
                        )
                ) ++
                [ td [ class "text-right", style "min-width" "10em" ]
                    [ Cart.addToCartButton (Session.getCart model.session) result.files |> Html.map CartMsg ]
                ])
    in
    SortableTable.view
        { tableAttrs = [ class "table table-sm table-striped", style "font-size" "0.85em" ] }
        model.sampleTableState
        columns
        (model.searchResponse
            |> RemoteData.map .sampleResults
            |> RemoteData.toMaybe
            |> Maybe.withDefault []
            |> List.map mkTr
        )
        []


viewFileResults : Model -> Html Msg
viewFileResults model =
    let
        mkTh index label =
            let
                pos =
                    index + 1

                lbl =
                    String.Extra.toTitleCase
                        (if pos == abs model.fileTableState.sortCol then
                            label ++ " " ++ (viewUpDownArrow model.fileTableState.sortDir )
                        else
                            label
                        )
            in
            th [ style "cursor" "pointer", style "max-width" maxResultColWidth, onClick (SetFileSortPos pos) ] [ text lbl ]

        colHeaders =
            [ "Project Name"
            , "Sample ID"
            , "Source"
            , "Strategy"
            , "Selection"
            , "Layout"
            , "File Name"
            ]

        addToCartTh =
            th []
                [ Cart.addAllToCartButton (Session.getCart model.session) Nothing
                    (model.searchResponse
                        |> RemoteData.map .fileResults
                        |> RemoteData.toMaybe
                        |> Maybe.withDefault []
                        |> List.map .fileId
                    )
                    |> Html.map CartMsg
                ]

        columns =
            List.indexedMap mkTh colHeaders
                ++ [ addToCartTh ]

        mkTd label =
            td [ style "max-width" maxResultColWidth ] [ text label ]

        mkRow result =
            let
                basename path =
                    String.split "/" path |> List.reverse |> List.head |> Maybe.withDefault ""
            in
            tr []
                [ mkTd result.projectName
                , td [] [ a [ Route.href (Route.Sample result.sampleId) ] [ text result.sampleAccn ] ]
                , mkTd result.source
                , mkTd result.strategy
                , mkTd result.selection
                , mkTd result.layout
                , td [] [ a [ href (dataCommonsUrl ++ result.fileUrl), target "_blank" ] [ text <| basename result.fileUrl ] ]
                , td [] [ Cart.addToCartButton (Session.getCart model.session) [ result.fileId ] |> Html.map CartMsg ]
                ]
    in
    SortableTable.view
        { tableAttrs = [ class "table table-sm table-striped", style "font-size" "0.85em" ] }
        model.fileTableState
        columns
        (model.searchResponse
            |> RemoteData.map .fileResults
            |> RemoteData.toMaybe
            |> Maybe.withDefault []
            |> List.map mkRow
        )
        []


--TODO integrate into SortableTable
viewUpDownArrow : SortableTable.Direction -> String
viewUpDownArrow sortDir =
    if sortDir == SortableTable.ASC then
        String.fromChar (Char.fromCode 9650)
    else
        String.fromChar (Char.fromCode 9660)


viewMap : Bool -> Bool -> Html Msg
viewMap showMap mapLoaded =
    -- Map element must always be rendered for port to work properly
    let
        hideOrShow =
            if showMap then
                "block"
            else
                "none"
    in
    GMap.view [ style "display" hideOrShow, style "height" "50vh", style "width" "100%", style "margin-bottom" "0.85em", classList [("border", mapLoaded)] ] []


viewFormatButton : Html Msg
viewFormatButton =
    button [ type_ "button", class "btn", style "border" "1px solid lightgray", style "color" "gray", attribute "data-toggle" "dropdown" ]
        [ Icon.cog ]


-- Translate purl to label (for Biome and Env Material terms) if argument is a PURL
purlToLabel : SearchTerm -> String -> String
purlToLabel term s =
    Dict.get s term.purlLabels |> Maybe.withDefault s
