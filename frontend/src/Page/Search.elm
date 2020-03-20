module Page.Search exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (class, classList, style, href, disabled, type_, value, placeholder, target, checked, attribute)
import Html.Events exposing (onClick, onInput, onCheck)
import Http
import RemoteData exposing (RemoteData(..))
import Json.Encode as Encode
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
import Page exposing (viewBlank, viewSpinner, viewDialog)
import Project
import Sample
import Search exposing (SearchResponse, SearchResults(..), SampleResult, FileResult, SearchResultValue(..), Value(..), Filter, FilterValue(..), SearchTerm, PURL, Distribution, defaultSearchTerm)
import RemoteFile
import SortableTable
import BarChart
import Cart
import Icon
import Config exposing (dataCommonsUrl)
--import Debug exposing (toString)



-- CONSTANTS --


myLocale =
    { usLocale | decimals = 0 }


defaultPageSize =
    20


maxNumPanelOptions =
    4


minNumPanelOptionsForSearchBar =
    100


purlDepth =
    "http://purl.obolibrary.org/obo/ENVO_3100031"


--purlDepthMin =
--    "http://purl.obolibrary.org/obo/PMO_00000172"


--purlDepthMax =
--    "http://purl.obolibrary.org/obo/PMO_00000052"


-- Need to resolve time zone issue with fields that reference this PURL
--purlDateTimeLocal =
--    "http://purl.obolibrary.org/obo/PMO_00000081"


purlDateTimeISO =
    "http://purl.obolibrary.org/obo/OBI_0001619"


--purlDateTimeISOStart =
--    "http://purl.obolibrary.org/obo/PMO_00000008"


--purlDateTimeISOEnd =
--    "http://purl.obolibrary.org/obo/PMO_00000009"


purlBiome =
    "http://purl.obolibrary.org/obo/ENVO_00000428"


purlEnvironmentalFeature =
    "http://purl.obolibrary.org/obo/ENVO_00002297"


purlEnvironmentalMaterial =
    "http://purl.obolibrary.org/obo/ENVO_00010483"


purlLocation = -- Not a real PURL, but a unique id for this term used in search query string
    "location"


purlProject = -- Not a real PURL, but a unique id for this term used in search query string
    "project"


purlFileSource =
    "source"


purlFileStrategy =
    "strategy"


purlFileSelection =
    "selection"


purlFileLayout =
    "layout"


permanentSampleTerms =
    [ purlLocation
    , purlDepth
    , purlDateTimeISO
    , purlProject
    ]


initialSelectedSampleTerms =
    [ purlBiome
    , purlEnvironmentalFeature
    , purlEnvironmentalMaterial
    ]


permanentFileTerms =
    [ purlFileSource
    , purlFileStrategy
    , purlFileSelection
    , purlFileLayout
    ]



-- MODEL --


type alias Model =
    { session : Session

    -- Search terms and filters
    , allTerms: List SearchTerm -- list of available terms to add to search
    --, selectedTerms : List PURL -- list of selected terms (needed to remember order added)
    , filters : List Filter

    -- There could be additional datetime fields
    , dateRangePickers : Dict PURL DateRangePicker

    , showParamSearchDropdown : Bool
    , paramSearchInputVal : String

    -- Dialog states
    , dialogState : DialogState
    , dialogSearchInputVal : String

    -- Search result state
    --, doSearch : Bool
    --, searchStartTime : Int -- milliseconds
    --, isSearching : Bool
    , searchState : SearchState
    , sampleResults : Maybe (List SampleResult)
    , fileResults : Maybe (List FileResult)
    , mapResults : Encode.Value --List MapResult
    , summaryResults : Maybe (List Distribution)
    , sampleResultCount : Int
    , fileResultCount : Int
    , sampleTableState : SortableTable.State
    , fileTableState : SortableTable.State
    --, errorMsg : Maybe String
    , searchTab : String
    , resultTab : String
    , pageNum : Int
    , pageSize : Int
    , showMap : Bool
    , mapLoaded : Bool
    , previousSearchParams : List (String, String)
    }


type SearchState
    = SearchNot          -- Idle
    | SearchInit         -- Page loading
    | SearchPending Int  -- Time elapsed in milliseconds
    | Searching          -- Search request in progress
    | SearchError String -- Search request failed


type DialogState --TODO combine StringFilterDialog/ProjectFilterDialog/FileFilterDialog and FilterChartDialog/ProjectSummaryDialog/FileFilterDialog
    = DialogClosed
    | AddFilterDialog
    | StringFilterDialog Filter
    | FilterChartDialog SearchTerm
    | ProjectFilterDialog
    | ProjectSummaryDialog
    | FileFilterDialog Filter
    | FileSummaryDialog Filter


type alias DateRangePicker =
    { start : DatePicker
    , end : DatePicker
    }


init : Session -> ( Model, Cmd Msg )
init session =
    (
        { session = session

        -- Search filters and selected values
        , allTerms = []
        --, selectedTerms = initialSelectedTerms
        , filters = []

        , dateRangePickers = Dict.empty

        , showParamSearchDropdown = False
        , paramSearchInputVal = ""

        -- Dialog states
        , dialogState = DialogClosed
        , dialogSearchInputVal = ""

        -- Search result state
        --, doSearch = False -- initial search is activated by GetSearchTermCompleted
        --, searchStartTime = 0
        --, isSearching = True
        , searchState = SearchInit
        , sampleResults = Nothing
        , fileResults = Nothing
        , mapResults = Encode.object []
        , summaryResults = Nothing
        , sampleResultCount = 0
        , fileResultCount = 0
        , sampleTableState = SortableTable.initialState
        , fileTableState = SortableTable.initialState
        --, errorMsg = Nothing
        , searchTab = "Samples"
        , resultTab = "Samples"
        , pageNum = 0
        , pageSize = defaultPageSize
        , showMap = True
        , mapLoaded = False
        , previousSearchParams = []
        }
    , Cmd.batch
        [ Date.today |> Task.perform InitDatePickers
        , Sample.fetchAllSearchTerms |> Http.toTask |> Task.attempt GetAllSearchTermsCompleted
        , Sample.fetchSearchTerms initialSelectedSampleTerms |> Http.toTask |> Task.attempt GetSearchTermsCompleted
        , Project.fetchCounts |> Http.toTask |> Task.attempt GetProjectCountsCompleted
        , RemoteFile.fetchProperties |> Http.toTask |> Task.attempt GetFilePropertiesCompleted
        , GMap.removeMap "" -- workaround for blank map on navigating back to this page
        , GMap.changeMapSettings (GMap.Settings True True False True |> GMap.encodeSettings)
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 750 InputTimerTick -- milliseconds
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
    --| SetLocationFilterValue LocationFilterValue
    --| SetProjectFilterValue String Bool
    --| SetFileFilterValue String String Bool
    | SetSearchTab String
    | SetResultTab String
    | SetSampleSortPos Int
    | SetFileSortPos Int
    | SetPageSize Int
    | SetPageNum Int
    | InputTimerTick Time.Posix
    | Search Int Bool
    | SampleSearchCompleted (Result Http.Error SearchResponse)
    | FileSearchCompleted (Result Http.Error SearchResponse)
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
    case msg of
        GetProjectCountsCompleted (Ok counts) ->
            let
                projectFilter =
                    Filter { defaultSearchTerm | id = purlProject, label = "Project", distribution = counts } NoValue

                newFilters =
                    List.append model.filters [ projectFilter ]
            in
            ( { model | filters = newFilters }, Cmd.none )

        GetProjectCountsCompleted (Err error) ->
            ( { model | searchState = SearchError (Error.toString error) }, Cmd.none )

        GetFilePropertiesCompleted (Ok props) ->
            let
                fileFilters =
                    props
                        |> List.map
                            (\(id,dist) ->
                                Filter { defaultSearchTerm | id = id, label = id, distribution = dist } NoValue
                            )

                newFilters =
                    List.append model.filters fileFilters
            in
            ( { model | filters = newFilters }, Cmd.none )

        GetFilePropertiesCompleted (Err error) ->
            ( { model | searchState = SearchError (Error.toString error) }, Cmd.none )

        GetAllSearchTermsCompleted (Ok terms) ->
            ( { model | allTerms = terms }, Cmd.none )

        GetAllSearchTermsCompleted (Err error) ->
            ( { model | searchState = SearchError (Error.toString error) }, Cmd.none )

        GetSearchTermsCompleted (Ok terms) ->
            let
                --selectedParams =
                --    List.singleton term.id |> List.append model.selectedParams |> List.Extra.unique -- cannot use Set because it doesn't preserve order

                newFilter term =
                    { term = term
                    , value =
                        case term.type_ of
                            "number" ->
                                RangeValue (String.fromFloat term.min) (String.fromFloat term.max)

                            _ ->
                                NoValue
                    }

                --selectedVals =
                --    Dict.insert term.id val model.selectedVals

                newFilters =
                    --Dict.insert term.id term model.selectedTerms
                    List.append model.filters (List.map newFilter terms)
            in
            --( { model | doSearch = True, selectedParams = selectedParams, selectedTerms = selectedTerms, selectedVals = selectedVals }, Cmd.none )
            ( { model | searchState = SearchPending 0, filters = newFilters }, Cmd.none )

        GetSearchTermsCompleted (Err error) ->
            ( { model | searchState = SearchError (Error.toString error) }, Cmd.none )

        ClearFilters ->
            let
                terms =
                    permanentSampleTerms ++ initialSelectedSampleTerms ++ permanentFileTerms

                newFilters =
                    model.filters
                        |> List.filter (\f -> List.member f.term.id terms)
                        |> List.map (\f -> { f | value = NoValue })
            in
            ( { model
                | searchState = SearchPending 0
                , filters = newFilters
                , sampleTableState = SortableTable.initialState
                , fileTableState = SortableTable.initialState
              }
            , GMap.setLocation Nothing
            )

        AddFilter id ->
            let
                getTerm =
                    Sample.fetchSearchTerms [ id ] |> Http.toTask
            in
            ( { model | showParamSearchDropdown = False, paramSearchInputVal = "", dialogState = DialogClosed }
            , Task.attempt GetSearchTermsCompleted getTerm
            )

        RemoveFilter id ->
            let
                newFilters =
                    model.filters |> List.filter (\f -> f.term.id == id)
            in
            ( { model
                | searchState = SearchPending 0
                , filters = newFilters
                , sampleTableState = SortableTable.initialState
                , fileTableState = SortableTable.initialState
              }
            , Cmd.none
            )

        OpenDialog state ->
            ( { model | dialogState = state }, Cmd.none )

        CloseDialog ->
            ( { model | dialogState = DialogClosed }, Cmd.none )

        --OpenFilterChartDialog id ->
        --    let
        --        maybeTerm =
        --            Dict.get id model.selectedTerms
        --    in
        --    ( { model | chartFilterDialogTerm = maybeTerm }, Cmd.none )
        --
        --CloseFilterChartDialog ->
        --    ( { model | chartFilterDialogTerm = Nothing }, Cmd.none )
        --
        --OpenProjectChartDialog ->
        --    ( { model | showProjectSummaryDialog = True }, Cmd.none )
        --
        --CloseProjectChartDialog ->
        --    ( { model | showProjectSummaryDialog = False }, Cmd.none )

        SetParamSearchInput val ->
            ( { model | paramSearchInputVal = val }, Cmd.none )

        ShowParamSearchDropdown ->
            ( { model | showParamSearchDropdown = not model.showParamSearchDropdown }, Cmd.none )

        --OpenAddFilterDialog ->
        --    ( { model | showAddFilterDialog = True, dialogSearchInputVal = "" }, Cmd.none )
        --
        --CloseAddFilterDialog ->
        --    ( { model | showAddFilterDialog = False }, Cmd.none )

        SetDialogSearchInput val ->
            ( { model | dialogSearchInputVal = val }, Cmd.none )

        --OpenStringFilterDialog term ->
        --    ( { model | stringFilterDialogTerm = Just term }, Cmd.none )
        --
        --CloseStringFilterDialog ->
        --    ( { model | stringFilterDialogTerm = Nothing }, Cmd.none )
        --
        --OpenProjectFilterDialog ->
        --    ( { model | showProjectFilterDialog = True }, Cmd.none )
        --
        --CloseProjectFilterDialog ->
        --    ( { model | showProjectFilterDialog = False }, Cmd.none )

        SetSearchFilterValue id val ->
            let
                searchState =
                    if val == "" || String.length val > 2 then
                        SearchPending 0
                    else
                        SearchNot

                newVal =
                    if val == "" then
                        NoValue
                    else
                        SearchValue val

                newFilters =
                    Search.updateFilterValue id newVal model.filters
            in
            ( { model | searchState = searchState, filters = newFilters }, Cmd.none )

        SetStringFilterValue id val selected ->
            let
                curVal =
                    Search.getFilterValue id model.filters

                newVal =
                    case curVal of
                        NoValue ->
                            if selected then
                                SingleValue val
                            else
                                NoValue

                        SingleValue val1 -> --FIXME merge into MultipleValues case?
                            if selected then
                                MultipleValues [val1, val]
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

                newFilters =
                    Search.updateFilterValue id (newVal ) model.filters
            in
            ( { model | searchState = SearchPending 0, filters = newFilters }, Cmd.none )

        SetFilterValue id val ->
            let
                newFilters =
                    Search.updateFilterValue id val model.filters
            in
            ( { model | searchState = SearchPending 0, filters = newFilters }, Cmd.none )

        SetSearchTab label ->
            ( { model | searchTab = label }, Cmd.none )

        SetResultTab label ->
            let
                searchState =
                    if (label == "Samples" && model.sampleResults == Nothing) || (label == "Files" && model.fileResults == Nothing) then
                        SearchPending 0
                    else
                        SearchNot
            in
            ( { model | searchState = searchState, resultTab = label }, Cmd.none )

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
            ( { model | searchState = SearchPending 0, sampleTableState = newTableState }, Cmd.none )

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
            ( { model | searchState = SearchPending 0, fileTableState = newTableState }, Cmd.none )

        SetPageSize size ->
            ( { model | searchState = SearchPending 0, pageSize = size }, Cmd.none )

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

        InputTimerTick time ->
            case model.searchState of
                SearchPending elapsed ->
                    if Time.posixToMillis time - elapsed >= 1000 then -- 1 second
                        update (Search 0 False) model
                    else
                        ( model, Cmd.none )
                _ ->
                    ( model, Cmd.none )
            --if model.doSearch && Time.posixToMillis time - model.searchStartTime >= 1000 then -- 1 second
            --    update (Search 0 False) model
            --else
            --    ( model, Cmd.none )

        Search newPageNum download ->
            case generateQueryParams model.filters of
                Ok queryParams ->
                    let
                        ( result, sortPos, cmd ) =
                            if model.resultTab == "Files" then
                                ( "file", model.fileTableState.sortCol * (SortableTable.directionToInt model.fileTableState.sortDir), FileSearchCompleted )
                            else
                                ( "sample", model.sampleTableState.sortCol * (SortableTable.directionToInt model.sampleTableState.sortDir), SampleSearchCompleted )

                        summaryParams =
                            ( "summary"
                            , model.filters
                                |> List.filter (\f -> not <| List.member f.term.id permanentSampleTerms)
                                |> List.map (.term >> .id)
                                |> String.join ","
                            )

                        controlParams =
                            generateControlParams result [] sortPos model.pageSize (model.pageSize * newPageNum) model.showMap

                        allParams =
                            summaryParams ::
                            queryParams ++
                            controlParams
                    in
                    if download || allParams /= model.previousSearchParams then
                        ( { model
                            --| doSearch = False
                            --, isSearching = True
                            | searchState = Searching
                            , pageNum = newPageNum
                            , previousSearchParams = allParams
                          }
                        , if download then
                            Search.searchDownloadRequest allParams |> Http.toTask |> Task.attempt DownloadSearchCompleted
                          else
                            Search.searchRequest allParams |> Http.toTask |> Task.attempt cmd
                        )
                    else -- do nothing
                        ( { model | searchState = SearchNot }, Cmd.none )

                Err error ->
--                    let
--                        _ = Debug.log "Error generating query params" "" --(toString error)
--                    in
                    ( model, Cmd.none )

        SampleSearchCompleted (Ok response) ->
            let
                sampleResults =
                    case response.results of
                        SampleSearchResults results ->
                            Just results

                        _ ->
                            Nothing
            in
            ( { model
                | sampleResultCount = response.count
                , sampleResults = sampleResults
                , summaryResults = Just response.summary
                , mapResults = response.map
                --, errorMsg = response.error
                , searchState = SearchNot
              }
            , GMap.loadMap response.map
            )

        SampleSearchCompleted (Err error) ->
            ( { model | searchState = SearchError (Error.toString error) }, Cmd.none )

        FileSearchCompleted (Ok response) ->
            let
                fileResults =
                    case response.results of
                        FileSearchResults results ->
                            Just results

                        _ ->
                            Nothing
            in
            ( { model
                | fileResultCount = response.count
                , fileResults = fileResults
--                , mapResults = response.map
                , searchState = SearchNot
              }
            , Cmd.none
            )

        FileSearchCompleted (Err error) ->
            ( { model | searchState = SearchError (Error.toString error) }, Cmd.none ) --TODO

        DownloadSearchCompleted (Ok response) ->
            ( { model | searchState = SearchNot }
            , Download.string "PM_Search_Results.tsv" "text/tab-separated-values" response
            )

        DownloadSearchCompleted (Err error) ->
            ( { model | searchState = SearchError (Error.toString error) }, Cmd.none )

        ToggleMap ->
            let
                showMap =
                    not model.showMap
            in
            ( { model | showMap = showMap }, Cmd.none )

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
                    Search.updateFilter purlLocation locationFilter model.filters
            in
            ( { model | searchState = SearchPending 0, filters = newFilters }, Cmd.none )

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

        SetStartDatePicker id val subMsg -> --TODO merge code in common with SetEndDatePicker into function
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
                                            val

                                Nothing ->
                                    val

                        newFilters =
                            Search.updateFilterValue purlDateTimeISO newDateVal model.filters
                    in
                    ({ model
                        | searchState = if newDate /= Nothing then SearchPending 0 else SearchNot
                        , filters = newFilters
                        , dateRangePickers = Dict.insert id newDateRangePicker model.dateRangePickers
                    }
                    , Cmd.none)

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
                                            val

                                Nothing ->
                                    val

                        newFilters =
                            Search.updateFilterValue purlDateTimeISO newDateVal model.filters
                    in
                    ({ model
                        | searchState = if newDate /= Nothing then SearchPending 0 else SearchNot
                        , filters = newFilters
                        , dateRangePickers = Dict.insert id newDateRangePicker model.dateRangePickers
                    }
                    , Cmd.none)

                Nothing -> -- should never happen
                    ( model, Cmd.none )

        CartMsg subMsg ->
            let
                newCart =
                    Cart.update subMsg (Session.getCart model.session)

                newSession =
                    Session.setCart model.session newCart
            in
            ( { model | session = newSession }
            , Cmd.batch
                [ --Cmd.map CartMsg subCmd
                Cart.store newCart
                ]
            )


--validLocationParam : FilterValue -> Bool
--validLocationParam val =
--    case val of
--        LatLngRadiusValue (lat,lng) radius ->
--            defined lat && defined lng
--
--        LonghurstValue s ->
--            defined s
--
--        NoValue ->
--            True
--
--        _ ->
--            False


--encodeLocationParam : FilterValue -> String
--encodeLocationParam val =
--    case val of
--        LatLngRadiusValue (lat,lng) radius ->
--            let
--                r =
--                    if radius == "" then
--                        "0"
--                    else
--                        radius
--            in
--            "[" ++ lat ++ "," ++ lng ++ "," ++ r ++ "]"
--
--        LonghurstValue s ->
--            s
--
--        _ ->
--            ""


-- TODO refactor/simplify
generateQueryParams : List Filter -> Result String (List (String, String))
generateQueryParams termFilters =
    --if locationVal == NoLocationValue && projectFilter.value == NoValue && Dict.isEmpty vals then
    --    Ok []
    --else
        let
            termParams =
                termFilters
                    --|> List.filter (\f -> f.term.id == purlProject && f.value == NoValue) -- remove project if no value
                    |> List.map (\f -> Tuple.pair f.term.id (Search.filterValueToString f.value))

            --locParam =
            --    if validLocationParam locationVal then
            --        [ ("location", encodeLocationParam locationVal) ]
            --    else
            --        []

            --depthParam =
            --    case List.filter (\f -> f.term.id == purlDepth) termFilters of
            --        [ filter ] ->
            --            if validParam filter.value then
            --                [ ( purlDepth, encodeFilterValue filter.value )
            --                --, ( "|" ++ purlDepthMin, encodeFilterValue term.value )
            --                --, ( "|" ++ purlDepthMax, encodeFilterValue term.value )
            --                ]
            --            else
            --                []
            --
            --        _ ->
            --            []

            --datetimeParam =
            --    case List.filter (\f -> f.term.id == purlDateTimeISO) termFilters of
            --        [ filter ] ->
            --            if validParam filter.value then
            --                let
            --                    encodedVal =
            --                        encodeFilterValue filter.value
            --                in
            --                [ ( "|" ++ purlDateTimeISO, encodedVal )
            --                , ( "|" ++ purlDateTimeISOStart, encodedVal )
            --                , ( "|" ++ purlDateTimeISOEnd, encodedVal )
            --                ]
            --            else
            --                []
            --
            --        _ ->
            --            []

            --projectParam =
            --    if projectFilter.value /= NoValue then
            --        [ ("project", encodeFilterValue projectFilter.value) ]
            --    else
            --        []

            --fileParams =
            --    fileFilters
            --        |> List.filter (\f -> f.value /= NoValue)
            --        |> List.map (\f -> Tuple.pair f.term.id (encodeFilterValue f.value))
        in
        --List.concat [ projectParam, fileParams, locParam, depthParam, datetimeParam, termParams ] |> Ok
        Ok termParams
--    else
--        Err "Invalid query parameter"


generateControlParams : String -> List String -> Int -> Int -> Int -> Bool -> List (String, String)
generateControlParams result columns sortPos limit offset showMap =
    [ ("result", result)
    , ("columns", String.join "," columns)
    , ("sort", String.fromInt sortPos)
    , ("limit", String.fromInt limit)
    , ("offset", String.fromInt offset)
    , ("map", if showMap then "1" else "0")
    ]



-- VIEW --


view : Model -> Html Msg
view model =
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
        DialogClosed ->
            viewBlank

        AddFilterDialog ->
            viewAddFilterDialog model.allTerms model.dialogSearchInputVal

        StringFilterDialog filter ->
            viewStringFilterDialog filter

        FilterChartDialog term ->
            viewSearchTermSummaryDialog term

        ProjectFilterDialog ->
            case model.filters |> List.filter (\f -> f.term.id == purlProject) |> List.head of
                Just projectFilter ->
                    viewStringFilterDialog projectFilter

                Nothing ->
                    viewBlank

        ProjectSummaryDialog ->
            case model.filters |> List.filter (\f -> f.term.id == purlProject) |> List.head of
                Just projectFilter ->
                    viewSearchTermSummaryDialog projectFilter.term

                Nothing ->
                    viewBlank

        FileFilterDialog filter ->
            viewStringFilterDialog filter

        FileSummaryDialog filter ->
            viewBlank --viewFilleSummaryDialog model.projectCounts --TODO


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
            viewFileSearchPanel (model.filters |> List.filter (\f -> List.member f.term.id permanentFileTerms))
          else
            text ""
        ]


viewSampleSearchPanel : Model -> Html Msg
viewSampleSearchPanel model =
    div []
        [ view4DPanel model
        , viewAddedFiltersPanel model.filters model.dateRangePickers
        , viewAddFilterPanel model.showParamSearchDropdown model.paramSearchInputVal model.allTerms model.filters
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
            Search.getFilterValue purlLocation model.filters

        depthVal =
            Search.getFilterValue purlDepth model.filters

        datetimeVal =
            Search.getFilterValue purlDateTimeISO model.filters

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
                                        List.concat
                                        [ viewDateTimeFilterInput picker purlDateTimeISO datetimeVal
                                        , [ viewDateTimeFilterFormatOptions purlDateTimeISO datetimeVal ]
                                        ]

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


redundantTerms =
    [ "latitude coordinate measurement datum"
    , "longitude coordinate measurement datum"
    , "latitude coordinate measurement datum start"
    , "latitude coordinate measurement datum stop"
    , "zero-dimensional temporal region"
    , "depth of water"
    , "specimen collection time measurement datum start"
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
            a [ classList [ ("dropdown-item", True), ("disabled", dis) ], href "", onClick (AddFilter term.id) ] [ term.label |> String.Extra.toSentenceCase |> text ]

        removeRedundantTerms term =
            redundantTerms
                |> List.member (String.toLower term.label)
                |> not

        filterOnSearch term =
            let
                lowerSearchVal =
                    String.toLower searchVal
            in
            String.contains lowerSearchVal (String.toLower term.label)
                || String.contains lowerSearchVal (String.toLower term.id)

        options =
            allTerms
                |> List.filter removeRedundantTerms
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
                [ button [ class "btn btn-outline-secondary", type_ "button", onClick (OpenDialog AddFilterDialog) ] [ text "?" ]
                , button [ class "btn btn-outline-secondary dropdown-toggle", type_ "button", onClick ShowParamSearchDropdown ] []
                , div [ class "dropdown-menu", classList [("show", show)], style "position" "absolute", style "left" "0px", style "max-height" "30vh", style "overflow-y" "auto" ] options
                ]
            ]
        ]


viewAddFilterDialog : List SearchTerm -> String -> Html Msg
viewAddFilterDialog allTerms searchVal =
    let
        removeRedundantTerms term =
            redundantTerms
                |> List.member (String.toLower term.label)
                |> not

        filterOnSearch term =
            String.contains (String.toLower searchVal) (String.toLower term.label)
                || String.contains (String.toLower searchVal) (String.toLower term.id)

        terms =
            allTerms
                |> List.filter removeRedundantTerms
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
              else if count == 1 then
                (String.fromInt count) ++ " result" |> text
              else
                (String.fromInt count) ++ " results" |> text
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
        [ label [] [ numValues |> toFloat |> format myLocale |> text, text " unique values" ]
        , div [ class "input-group input-group-sm" ]
            [ input [ type_ "text", class "form-control", placeholder "Search ...", value val2, onInput (SetSearchFilterValue filter.term.id) ] [] ]
        ]


--viewProjectPanel : Filter -> Html Msg --TODO merge with viewFileFormatPanel/viewStringFilterPanel
--viewProjectPanel filter =
--    let
--        viewRow vals ( lbl, num ) =
--            let
--                isChecked =
--                    List.member lbl vals
--            in
--            div []
--                [ div [ class "form-check form-check-inline" ]
--                    [ input [ class "form-check-input", type_ "checkbox", checked isChecked, onCheck (SetProjectFilterValue lbl) ] []
--                    , label [ class "form-check-label" ] [ text lbl ]
--                    ]
--                , div [ class "badge badge-secondary float-right" ]
--                    [ num |> toFloat |> format myLocale |> text ]
--                ]
--
--        truncatedOptions =
--            filter.distribution
--                |> List.sortWith sortBySelected --|> List.sortBy Tuple.second
--                |> List.take maxNumPanelOptions
--
--        sortBySelected a b =
--            case ( isSelected (Tuple.first a), isSelected (Tuple.first b) ) of
--                (True, False) ->
--                    LT
--
--                (False, True) ->
--                    GT
--
--                (_, _) ->
--                    sortByCount a b
--
--        sortByCount a b =
--            case compare (Tuple.second a) (Tuple.second b) of
--                LT -> GT
--                EQ -> EQ
--                GT -> LT
--
--        isSelected lbl =
--            List.member lbl filter.values
--
--        numOptions =
--            List.length filter.distribution
--
--        numMore =
--            numOptions - maxNumPanelOptions
--    in
--    viewPanel "" "Project" "" "" Nothing (Just (OpenDialog ProjectSummaryDialog)) Nothing
--        [ div [] (List.map (viewRow filter.values) truncatedOptions)
--        , if numMore > 0 then
--            button [ class "btn btn-sm btn-link float-right", onClick (OpenDialog ProjectFilterDialog) ]
--                [ String.fromInt numMore ++ " More ..." |> text ]
--          else
--            viewBlank
--        ]


--viewProjectFilterDialog : Filter -> Html Msg --TODO merge with viewStringFilterDialog/viewFileFilterDialog
--viewProjectFilterDialog filter =
--    let
--        options =
--            filter.term.distribution |> List.sortBy Tuple.first
--
--        isSelected lbl =
--            List.member lbl filter.value
--
--        viewRow (lbl, count) =
--            div []
--                [ div [ class "form-check form-check-inline" ]
--                    [ input [ class "form-check-input", type_ "checkbox", checked (isSelected lbl), onCheck (SetProjectFilterValue lbl) ] []
--                    , label [ class "form-check-label" ] [ text lbl ]
--                    ]
--                , div [ class "badge badge-secondary float-right" ]
--                    [ count |> toFloat |> format myLocale |> text ]
--                ]
--    in
--    viewDialog "Project"
--        [ div [ style "overflow-y" "auto", style "max-height" "50vh" ] (List.map viewRow options) ]
--        [ button [ type_ "button", class "btn btn-secondary", onClick CloseDialog ] [ text "Close" ] ]
--        CloseDialog


--viewFilePropertyPanel : Filter -> Html Msg --TODO merge with viewProjectPanel/viewStringFilterPanel
--viewFilePropertyPanel filter =
--    let
--        viewRow vals ( lbl, num ) =
--            let
--                isChecked =
--                    List.member lbl vals
--            in
--            div []
--                [ div [ class "form-check form-check-inline" ]
--                    [ input [ class "form-check-input", type_ "checkbox", checked isChecked, onCheck (SetFileFilterValue filter.id lbl) ] []
--                    , label [ class "form-check-label" ] [ text lbl ]
--                    ]
--                , div [ class "badge badge-secondary float-right" ]
--                    [ num |> toFloat |> format myLocale |> text ]
--                ]
--
--        truncatedOptions =
--            filter.distribution
--                |> List.sortWith sortBySelected --|> List.sortBy Tuple.second
--                |> List.take maxNumPanelOptions
--
--        sortBySelected a b =
--            case ( isSelected (Tuple.first a), isSelected (Tuple.first b) ) of
--                (True, False) ->
--                    LT
--
--                (False, True) ->
--                    GT
--
--                (_, _) ->
--                    sortByCount a b
--
--        sortByCount a b =
--            case compare (Tuple.second a) (Tuple.second b) of
--                LT -> GT
--                EQ -> EQ
--                GT -> LT
--
--        isSelected lbl =
--            List.member lbl filter.values
--
--        numOptions =
--            List.length filter.distribution
--
--        numMore =
--            numOptions - maxNumPanelOptions
--    in
--    viewPanel "" filter.id "" "" Nothing (Just (OpenDialog ProjectSummaryDialog)) Nothing
--        [ div [] (List.map (viewRow filter.values) truncatedOptions)
--        , if numMore > 0 then
--            button [ class "btn btn-sm btn-link float-right", onClick (OpenDialog (FileFilterDialog filter)) ]
--                [ String.fromInt numMore ++ " More ..." |> text ]
--          else
--            viewBlank
--        ]


--viewFileFilterDialog : Filter -> Html Msg --TODO merge with viewStringFilterDialog/viewProjectFilterDialog
--viewFileFilterDialog filter =
--    let
--        options =
--            filter.term.distribution |> List.sortBy Tuple.first
--
--        isSelected lbl =
--            case filter.value of
--                MultipleValues vals ->
--                    List.member lbl vals
--
--                _ ->
--                    False
--
--        viewRow (lbl, count) =
--            div []
--                [ div [ class "form-check form-check-inline" ]
--                    [ input [ class "form-check-input", type_ "checkbox", checked (isSelected lbl), onCheck (SetFileFilterValue filter.term.id lbl) ] []
--                    , label [ class "form-check-label" ] [ text lbl ]
--                    ]
--                , div [ class "badge badge-secondary float-right" ]
--                    [ count |> toFloat |> format myLocale |> text ]
--                ]
--    in
--    viewDialog filter.term.id
--        [ div [ style "overflow-y" "auto", style "max-height" "50vh" ] (List.map viewRow options) ]
--        [ button [ type_ "button", class "btn btn-secondary", onClick CloseDialog ] [ text "Close" ] ]
--        CloseDialog


viewStringFilterPanel : Filter -> Html Msg
viewStringFilterPanel filter =
    let
        numOptions =
            List.length filter.term.distribution
    in
    viewTermPanel filter.term
        [ div []
            (viewStringFilterOptions filter)
        , if numOptions > maxNumPanelOptions then
            button [ class "btn btn-sm btn-link float-right", onClick (OpenDialog (StringFilterDialog filter)) ]
                [ String.fromInt (numOptions - maxNumPanelOptions) ++ " More ..." |> text ]
          else
            viewBlank
        ]


viewStringFilterOptions : Filter -> List (Html Msg)
viewStringFilterOptions filter =
    let
        sortByCount a b =
            case compare (Tuple.second a) (Tuple.second b) of
                LT -> GT
                EQ -> EQ
                GT -> LT

        sortBySelected a b =
            case ( isStringFilterSelected (Tuple.first a) filter.value, isStringFilterSelected (Tuple.first b) filter.value ) of
                (True, False) ->
                    LT

                (False, True) ->
                    GT

                (_, _) ->
                    sortByCount a b

        numSelected =
            filter.term.distribution
                |> List.filter (\a -> isStringFilterSelected (Tuple.first a) filter.value)
                |> List.length

        truncatedOptions =
            filter.term.distribution
                |> List.sortWith sortBySelected
                |> List.take (Basics.max maxNumPanelOptions numSelected)

        viewRow (name, count) =
            let
                -- Translate purl to label (for Biome and Env Material terms)
                purlToLabel s =
                    Dict.get s filter.term.purlLabels |> Maybe.withDefault s
            in
            -- Using table layout to fix issue with wrapping rows
            table [ style "width" "100%" ]
                [ tr []
                    [ td []
                        [ div [ class "form-check form-check-inline" ]
                            [ input [ class "form-check-input", type_ "checkbox", checked (isStringFilterSelected name filter.value), onCheck (SetStringFilterValue filter.term.id name) ] []
                            , label [ class "form-check-label" ] [ name |> purlToLabel |> String.Extra.toSentenceCase |> text]
                            ]
                        ]
                    , td [ style "max-width" "3em" ]
                        [ div [ class "badge badge-secondary float-right align-top" ]
                            [ count |> toFloat |> format myLocale |> text ]
                        ]
                    ]
                ]
    in
    List.map viewRow truncatedOptions


isStringFilterSelected : String -> FilterValue -> Bool
isStringFilterSelected name val =
    (case val of
        SingleValue s ->
            List.singleton s

        MultipleValues l ->
            l

        _ ->
            []
    ) |> List.member name


viewStringFilterDialog : Filter -> Html Msg
viewStringFilterDialog filter =
    viewDialog (String.Extra.toTitleCase filter.term.label)
        [ div [ style "overflow-y" "auto", style "max-height" "50vh" ] (viewStringFilterOptions filter) ]
        [ button [ type_ "button", class "btn btn-secondary", onClick CloseDialog ] [ text "Close" ] ]
        CloseDialog


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
                [ label |> String.Extra.toSentenceCase |> text ]

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
                [ label |> String.Extra.toSentenceCase |> text ]

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
        latLngRadiusInput lat lng radius =
            [ input [ type_ "text", class "form-control", placeholder "lat", value lat, onInput (\p -> SetFilterValue purlLocation (LatLngRadiusValue (p,lng) radius)) ] []
            , input [ type_ "text", class "form-control", placeholder "lng", value lng, onInput (\p -> SetFilterValue purlLocation (LatLngRadiusValue (lat,p) radius)) ] []
            , input [ type_ "text", class "form-control", placeholder "radius", value radius, onInput (\p -> SetFilterValue purlLocation (LatLngRadiusValue (lat,lng) p)) ] []
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
    viewPanel term.id term.label term.unitId term.unitLabel (Just RemoveFilter) (Just (OpenDialog (FilterChartDialog term))) Nothing nodes


--TODO move coniguration params into type alias (like "type alias PanelConfig = {}")
viewPanel : PURL -> String -> PURL -> String -> Maybe (PURL -> Msg) -> Maybe Msg -> Maybe String -> List (Html Msg) -> Html Msg
viewPanel id title unitId unitLabel maybeRemoveMsg maybeOpenChartMsg maybeBgColor nodes =
    let
        header =
            h6 [ style "color" "darkblue"]
                [ text (String.fromChar (Char.fromCode 9660))
                , text " "
                , if id /= "" then
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
        (content, count) =
            case model.resultTab of
                "Samples" ->
                    ( viewSampleResults model
                    , model.sampleResultCount
                    )

                "Files" ->
                    ( viewFileResults model
                    , model.fileResultCount
                    )

                _ ->
                    ( viewSummary model
                    , model.sampleResultCount
                    )
    in
    div [ style "min-height" "50em" ]
        [ case model.searchState of
            SearchInit ->
                Page.viewSpinnerOverlay

            SearchError msg ->
                div [ class "alert alert-danger m-3" ]
                    [ p [] [ text "An error occurred:" ]
                    , p [] [ text msg ]
                    ]

            SearchPending _ ->
                Page.viewSpinnerOverlay

            Searching ->
                Page.viewSpinnerOverlay

            SearchNot ->
                if count == 0 then
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
                            [ viewPageSummary model.pageNum model.pageSize count
                            , content
                            , viewPageControls model.pageNum model.pageSize count
                            ]
                        ]
        ]


viewPageSummary : Int -> Int -> Int -> Html Msg
viewPageSummary curPageNum pageSize resultCount =
    div [ class "small ml-1", style "color" "dimgray" ]
        [ text "Showing "
        , curPageNum * pageSize + 1 |> Basics.max 1 |> String.fromInt |> text
        , text " - "
        , curPageNum * pageSize + pageSize |> Basics.max 1 |> Basics.min resultCount |> String.fromInt |> text
        , text " of "
        , resultCount |> toFloat |> format myLocale |> text
        , text " sample"
        , (if resultCount /= 1 then "s" else "") |> text
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
                --FIXME code below is a little kludgey
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
    case model.summaryResults of
        Nothing ->
            text "None"

        Just results ->
            let
                projectData =
                    List.head results |> Maybe.withDefault []

                termResults =
                    List.tail results |> Maybe.withDefault []

                termLabels =
                    model.filters
                        |> List.filter (\f -> f.term.id /= purlProject)
                        |> List.map (.term >> .label)
            in
            div [ style "margin" "1em" ]
                (viewSearchTermSummaryChart "project" projectData ::
                    (List.Extra.zip termLabels termResults
                        |> List.map (\(label, data) -> viewSearchTermSummaryChart label data)
                    )
                )


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
        -- Translate purl to label (for Biome and Env Material terms)
        purlToLabel s =
            Dict.get s term.purlLabels |> Maybe.withDefault s
    in
    viewDialog (String.Extra.toTitleCase term.label)
        [ div [ style "overflow-y" "auto", style "max-height" "50vh", style "text-align" "center", style "margin-top" "2em" ]
            [ viewSearchTermSummaryChart term.label (term.distribution |> List.map (Tuple.mapFirst purlToLabel)) ]
        ]
        [ button [ type_ "button", class "btn btn-secondary", onClick CloseDialog ]
            [ text "Close" ]
        ]
        CloseDialog


--viewProjectSummaryDialog : Distribution -> Html Msg
--viewProjectSummaryDialog dist =
--    viewDialog "Project"
--        [ div [ style "overflow-y" "auto", style "max-height" "50vh", style "text-align" "center", style "margin-top" "2em" ]
--            [ viewSearchTermSummaryChart "Project" dist ]
--        ]
--        [ button [ type_ "button", class "btn btn-secondary", onClick CloseDialog ]
--            [ text "Close" ]
--        ]
--        CloseDialog


viewSampleResults : Model -> Html Msg
viewSampleResults model =
    let
        maxColWidth = "8em"

        timeSpaceColHeaders = -- kinda kludgey, find a better way to order time/space headers
            let
                locationVal =
                    Search.getFilterValue purlLocation model.filters

                depthVal =
                    Search.getFilterValue purlDepth model.filters

                datetimeVal =
                    Search.getFilterValue purlDateTimeISO model.filters
            in
            List.concat
                [ if locationVal /= NoValue && Search.validFilterValue locationVal then
                    [ "Location" ]
                  else
                    []
                , if depthVal /= NoValue && Search.validFilterValue depthVal then
                    [ "Depth" ] --, "Min Depth", "Max Depth" ]
                  else
                    []
                , if datetimeVal /= NoValue && Search.validFilterValue datetimeVal then
                    [ "Date", "Start Date", "End Date" ]
                  else
                    []
                |> List.filter (\s -> Search.defined s)
                ]

        colHeaders =
            "Project Name" ::
            "Sample ID" ::
            timeSpaceColHeaders ++
            (model.filters
                |> List.filter (\f -> f.term.id /= purlProject) -- redundant project
                |> List.map
                    (\f ->
                        if f.term.unitLabel /= "" then
                            f.term.label ++ " (" ++ f.term.unitLabel ++ ")"
                        else
                            f.term.label
                    )
            )

        addToCartTh =
            th [ class "text-right", style "min-width" "10em" ]
                [ Cart.addAllToCartButton (Session.getCart model.session) Nothing
                    (model.sampleResults
                        |> Maybe.withDefault []
                        |> List.map .sampleId
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
            th [ style "cursor" "pointer", style "max-width" maxColWidth, onClick (SetSampleSortPos pos) ] [ text lbl ]

        mkTd label =
            td [ style "max-width" maxColWidth ] [ text label ]

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
                    [ Cart.addToCartButton (Session.getCart model.session) result.sampleId |> Html.map CartMsg ]
                ])
    in
    SortableTable.view
        { tableAttrs = [ class "table table-sm table-striped", style "font-size" "0.85em" ] }
        model.sampleTableState
        columns
        (model.sampleResults |> Maybe.withDefault [] |> List.map mkTr)
        []


viewFileResults : Model -> Html Msg
viewFileResults model =
    let
        maxColWidth = "8em"

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
            th [ style "cursor" "pointer", style "max-width" maxColWidth, onClick (SetFileSortPos pos) ] [ text lbl ]

        paramNames =
            [ "Project Name"
            , "Sample ID"
            , "File Format"
            , "File Type"
            , "Path"
            ]

--        addToCartTh =
--            th []
--                [ Cart.addAllToCartButton (Session.getCart model.session) Nothing
--                    (model.sampleResults
--                        |> Maybe.withDefault []
--                        |> List.map .sampleId
--                    )
--                    |> Html.map CartMsg
--                ]

        columns =
            List.indexedMap mkTh paramNames
--                ++ [ addToCartTh ]

        mkTd label =
            td [ style "max-width" maxColWidth ] [ text label ]

        mkRow result =
            tr []
                [ mkTd result.projectName
                , td [] [ a [ Route.href (Route.Sample result.sampleId) ] [ text result.sampleAccn ] ]
                , td [] [ text (String.Extra.toSentenceCase result.fileFormat) ]
                , td [] [ text (String.Extra.toSentenceCase result.fileType) ]
                , td [] [ a [ href (dataCommonsUrl ++ result.fileUrl), target "_blank" ] [ text result.fileUrl ] ]
--                , td [] [ Cart.addToCartButton (Session.getCart model.session) result.sampleId |> Html.map CartMsg ]
                ]
    in
    SortableTable.view
        { tableAttrs = [ class "table table-sm table-striped", style "font-size" "0.85em" ] }
        model.fileTableState
        columns
        (model.fileResults |> Maybe.withDefault [] |> List.map mkRow)
        []


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
                style "display" "block"
            else
                style "display" "none"
    in
    GMap.view [ hideOrShow, style "height" "50vh", style "width" "100%", style "margin-bottom" "0.85em", classList [("border", mapLoaded)] ] []


viewFormatButton : Html Msg
viewFormatButton =
    button [ type_ "button", class "btn", style "border" "1px solid lightgray", style "color" "gray", attribute "data-toggle" "dropdown" ]
        [ Icon.cog ]
