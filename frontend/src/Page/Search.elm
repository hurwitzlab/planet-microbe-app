module Page.Search exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (class, classList, style, href, disabled, type_, value, placeholder, target, checked, attribute)
import Html.Events exposing (onClick, onInput, onCheck)
import Http
import HttpBuilder
import Json.Encode as Encode exposing (Value, null)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
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
import SearchTerm exposing (SearchTerm, PURL, Distribution, defaultSearchTerm)
import RemoteFile
import SortableTable
import BarChart
import Cart
import Icon
import Config exposing (apiBaseUrl, dataCommonsUrl)
--import Debug exposing (toString)



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


initialTerms =
    [ purlBiome
    , purlEnvironmentalFeature
    , purlEnvironmentalMaterial
    ]



-- MODEL --


type alias Model =
    { session : Session

    -- Search terms and filters
    , allTerms: List SearchTerm -- list of available terms to add
    , selectedTerms : List PURL
    , termFilters : List Filter

    , locationVal : LocationFilterValue
    , projectFilter : Filter
    , fileFilters : List Filter
    , startDatePickers : Dict PURL DatePicker
    , endDatePickers : Dict PURL DatePicker

    , showParamSearchDropdown : Bool
    , paramSearchInputVal : String

    -- Dialog states
    , dialogState : DialogState
    , dialogSearchInputVal : String

    -- Search result state
    , doSearch : Bool
    , searchStartTime : Int -- milliseconds
    , isSearching : Bool
    , sampleResults : Maybe (List SampleResult)
    , fileResults : Maybe (List FileResult)
    , mapResults : Value --List MapResult
    , summaryResults : Maybe (List Distribution)
    , sampleResultCount : Int
    , fileResultCount : Int
    , sampleTableState : SortableTable.State
    , fileTableState : SortableTable.State
    , errorMsg : Maybe String
    , searchTab : String
    , resultTab : String
    , pageNum : Int
    , pageSize : Int
    , showMap : Bool
    , mapLoaded : Bool
    , previousSearchParams : List (String, String)
    }


type alias Filter =
    { term : SearchTerm
    , value : FilterValue
    }


projectSearchTerm : SearchTerm
projectSearchTerm =
    { defaultSearchTerm
        | id = "project"
        , label = "Project"
    }


type DialogState --TODO combine StringFilterDialog/ProjectFilterDialog/FileFilterDialog and FilterChartDialog/ProjectSummaryDialog/FileFilterDialog
    = DialogClosed
    | AddFilterDialog
    | StringFilterDialog Filter
    | FilterChartDialog SearchTerm
    | ProjectFilterDialog
    | ProjectSummaryDialog
    | FileFilterDialog Filter
    | FileSummaryDialog Filter


init : Session -> ( Model, Cmd Msg )
init session =
    --let
    --    ( startDatePicker, _ ) =
    --        DatePicker.init
    --
    --    ( endDatePicker, _ ) =
    --        DatePicker.init
    --in
    (
        { session = session

        -- Search filters and selected values
        , allTerms = []
        , selectedTerms = initialTerms
        , termFilters = []

        , locationVal = NoLocationValue
        , depthVal = NoValue
        , datetimeVal = NoValue
        , projectFilter = Filter projectSearchTerm NoValue

        , fileFilters = []
        --, startDatePickers = Dict.insert purlDateTimeISO startDatePicker Dict.empty
        --, endDatePickers = Dict.insert purlDateTimeISO endDatePicker Dict.empty

        , showParamSearchDropdown = False
        , paramSearchInputVal = ""

        -- Dialog states
        , dialogState = DialogClosed
        , dialogSearchInputVal = ""

        -- Search result state
        , doSearch = False -- initial search is activated by GetSearchTermCompleted
        , searchStartTime = 0
        , isSearching = True
        , sampleResults = Nothing
        , fileResults = Nothing
        , mapResults = Encode.object []
        , summaryResults = Nothing
        , sampleResultCount = 0
        , fileResultCount = 0
        , sampleTableState = SortableTable.initialState
        , fileTableState = SortableTable.initialState
        , errorMsg = Nothing
        , searchTab = "Samples"
        , resultTab = "Samples"
        , pageNum = 0
        , pageSize = defaultPageSize
        , showMap = True
        , mapLoaded = False
        , previousSearchParams = []
        }
    , Cmd.batch
        [ Sample.fetchSearchTerms |> Http.toTask |> Task.attempt GetAllSearchTermsCompleted
        , List.map Sample.fetchSearchTerm initialTerms |> List.map Http.toTask |> List.map (Task.attempt GetSearchTermCompleted) |> Cmd.batch
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
    | GetSearchTermCompleted (Result Http.Error SearchTerm)
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
    | SetLocationFilterValue LocationFilterValue
    | SetProjectFilterValue String Bool
    | SetFileFilterValue String String Bool
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
    | SetStartDatePicker PURL FilterValue DatePicker.Msg
    | SetEndDatePicker PURL FilterValue DatePicker.Msg
    | CartMsg Cart.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetProjectCountsCompleted (Ok counts) ->
            let
                projectFilter =
                    model.projectFilter

                projectTerm =
                    model.projectFilter.term
            in
            ( { model | projectFilter = { projectFilter | term = { projectTerm | distribution = counts } } }, Cmd.none )

        GetProjectCountsCompleted (Err error) ->
            ( { model | errorMsg = Just (Error.toString error), doSearch = False, isSearching = False }, Cmd.none )

        GetFilePropertiesCompleted (Ok props) ->
            let
                fileFilters =
                    props
                        |> List.map
                            (\(id,dist) ->
                                Filter { defaultSearchTerm | id = id, distribution = dist } NoValue
                            )
            in
            ( { model | fileFilters = fileFilters }, Cmd.none )

        GetFilePropertiesCompleted (Err error) ->
            ( { model | errorMsg = Just (Error.toString error), doSearch = False, isSearching = False }, Cmd.none )

        GetAllSearchTermsCompleted (Ok terms) ->
            ( { model | allTerms = terms }, Cmd.none )

        GetAllSearchTermsCompleted (Err error) ->
            ( { model | errorMsg = Just (Error.toString error), doSearch = False, isSearching = False }, Cmd.none )

        GetSearchTermCompleted (Ok term) ->
            let
                --selectedParams =
                --    List.singleton term.id |> List.append model.selectedParams |> List.Extra.unique -- cannot use Set because it doesn't preserve order

                val =
                    case term.type_ of
                        "number" ->
                            RangeValue (String.fromFloat term.min) (String.fromFloat term.max)

                        _ ->
                            NoValue

                --selectedVals =
                --    Dict.insert term.id val model.selectedVals

                filters =
                    --Dict.insert term.id term model.selectedTerms
                    List.append model.termFilters [ Filter term val ]
            in
            --( { model | doSearch = True, selectedParams = selectedParams, selectedTerms = selectedTerms, selectedVals = selectedVals }, Cmd.none )
            ( { model | doSearch = True, termFilters = filters }, Cmd.none )

        GetSearchTermCompleted (Err error) ->
            ( { model | errorMsg = Just (Error.toString error), doSearch = False, isSearching = False }, Cmd.none )

        ClearFilters ->
            --let
            --    newVals =
            --        model.selectedVals
            --            |> Dict.remove purlDepth
            --            |> Dict.remove purlDateTimeISO
            --            |> Dict.map (\k v -> NoValue)
            --in
            ( { model
                | doSearch = True
                , selectedTerms = initialTerms
                , termFilters = []
                , locationVal = NoLocationValue
                , projectFilter = Filter projectSearchTerm NoValue
                , fileFilters = []
                , sampleTableState = SortableTable.initialState
                , fileTableState = SortableTable.initialState
              }
            , GMap.setLocation Nothing
            )

        AddFilter id ->
            let
                getTerm =
                    Sample.fetchSearchTerm id |> Http.toTask
            in
            ( { model | showParamSearchDropdown = False, paramSearchInputVal = "", dialogState = DialogClosed }
            , Task.attempt GetSearchTermCompleted getTerm
            )

        RemoveFilter id ->
            let
                --newParams =
                --    model.selectedParams |> List.filter (\n -> n /= id) -- cannot use Set because it doesn't preserve order
                --
                --newTerms =
                --    Dict.remove id model.selectedTerms
                --
                --newVals =
                --    Dict.remove id model.selectedVals
                newFilters =
                    model.termFilters |> List.filter (\f -> f.term.id == id)
            in
            ( { model
                | doSearch = True
                --, selectedParams = newParams
                --, selectedTerms = newTerms
                --, selectedVals = newVals
                , termFilters = newFilters
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
                doSearch =
                    val == "" || String.length val > 2

                newVal =
                    if val == "" then
                        NoValue
                    else
                        SearchValue val

                --newVals =
                --    Dict.insert id newVal model.selectedVals

                termFilters =
                    model.termFilters
                        |> List.map
                            (\f ->
                                if f.term.id == id then
                                    { f | value = newVal }
                                else
                                    f
                            )
            in
            ( { model | doSearch = doSearch, termFilters = termFilters }, Cmd.none )

        SetStringFilterValue id val selected ->
            let
                newVal termVal =
                    case termVal of
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

                termFilters =
                    model.termFilters
                        |> List.map
                            (\f ->
                                if f.term.id == id then
                                    { f | value = newVal f.value }
                                else
                                    f
                            )

                --newVals =
                --    Dict.insert id newVal model.selectedVals
            in
            ( { model | doSearch = True, termFilters = termFilters }, Cmd.none )

        SetFilterValue id val ->
            let
                --newVals =
                --    Dict.insert id val model.selectedVals

                termFilters =
                    model.termFilters
                        |> List.map
                            (\f ->
                                if f.term.id == id then
                                    { f | value = val }
                                else
                                    f
                            )
            in
            ( { model | doSearch = True, termFilters = termFilters }, Cmd.none )

        SetLocationFilterValue val ->
            let
                cmd =
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
            in
            ( { model | doSearch = True, locationVal = val }, cmd )

        --SetProjectFilterValue val selected ->
        --    let
        --        newValues =
        --            model.projectFilter.values
        --                |> Set.fromList
        --                |> (if selected then Set.insert val else Set.remove val)
        --                |> Set.toList
        --
        --        projectFilter =
        --            model.projectFilter
        --    in
        --    ( { model | doSearch = True, projectFilter = { projectFilter | values = newValues } }, Cmd.none )
        --
        --SetFileFilterValue id val selected ->
        --    let
        --        newValues vals =
        --            vals
        --                |> Set.fromList
        --                |> (if selected then Set.insert val else Set.remove val)
        --                |> Set.toList
        --
        --        fileFilters =
        --            model.fileFilters
        --                |> List.map
        --                    (\f ->
        --                        if f.id == id then -- should only be one filter with this id value
        --                            { f | values = newValues f.values }
        --                        else
        --                            f
        --                    )
        --
        --    in
        --    ( { model | doSearch = True, fileFilters = fileFilters }, Cmd.none )

        SetSearchTab label ->
            ( { model | searchTab = label }, Cmd.none )

        SetResultTab label ->
            let
                doSearch =
                    (label == "Samples" && model.sampleResults == Nothing) || (label == "Files" && model.fileResults == Nothing)
            in
            ( { model | doSearch = doSearch, resultTab = label }, Cmd.none )

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
            ( { model | doSearch = True, sampleTableState = newTableState }, Cmd.none )

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
            ( { model | doSearch = True, fileTableState = newTableState }, Cmd.none )

        SetPageSize size ->
            ( { model | doSearch = True, pageSize = size }, Cmd.none )

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
            if model.doSearch && Time.posixToMillis time - model.searchStartTime >= 1000 then -- 1 second
                update (Search 0 False) model
            else
                ( model, Cmd.none )

        Search newPageNum download ->
            case generateQueryParams model.locationVal model.projectFilter model.fileFilters model.termFilters of
                Ok queryParams ->
                    let
                        ( result, sortPos, cmd ) =
                            if model.resultTab == "Files" then
                                ( "file", model.fileTableState.sortCol * (SortableTable.directionToInt model.fileTableState.sortDir), FileSearchCompleted )
                            else
                                ( "sample", model.sampleTableState.sortCol * (SortableTable.directionToInt model.sampleTableState.sortDir), SampleSearchCompleted )

                        allParams =
                            ("summary", model.termFilters |> List.map (.term >> .label) |> String.join "," ) ::
                            queryParams ++
                            (generateControlParams result [] sortPos model.pageSize (model.pageSize * newPageNum) model.showMap)
                    in
                    if download || allParams /= model.previousSearchParams then
                        ( { model
                            | doSearch = False
                            , isSearching = True
                            , pageNum = newPageNum
                            , previousSearchParams = allParams
                          }
                        , if download then
                            searchDownloadRequest allParams |> Http.toTask |> Task.attempt DownloadSearchCompleted
                          else
                            searchRequest allParams |> Http.toTask |> Task.attempt cmd
                        )
                    else -- do nothing
                        ( { model
                            | doSearch = False
                            , isSearching = False
                            }
                        , Cmd.none
                        )

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
                , errorMsg = response.error
                , isSearching = False
              }
            , GMap.loadMap response.map
            )

        SampleSearchCompleted (Err error) ->
            ( { model | errorMsg = Just (Error.toString error), doSearch = False, isSearching = False }, Cmd.none )

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
                , isSearching = False
              }
            , Cmd.none
            )

        FileSearchCompleted (Err error) ->
            ( { model | errorMsg = Just (Error.toString error), doSearch = False, isSearching = False }, Cmd.none ) --TODO

        DownloadSearchCompleted (Ok response) ->
            ( { model | isSearching = False }
            , Download.string "PM_Search_Results.tsv" "text/tab-separated-values" response
            )

        DownloadSearchCompleted (Err error) ->
            ( { model | errorMsg = Just (Error.toString error), doSearch = False, isSearching = False }, Cmd.none )

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
                            NoLocationValue
            in
            ( { model | doSearch = True, locationVal = newLocationVal }, Cmd.none )

        MapLoaded success ->
            ( { model | mapLoaded = True }, Cmd.none )

        SetStartDatePicker id val subMsg ->
            let
                startDatePicker =
                    Dict.get id model.startDatePickers
            in
            case startDatePicker of
                Just datePicker ->
                    let
                        ( newDatePicker, dateEvent ) =
                            DatePicker.update startDatePickerSettings subMsg datePicker

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

                        termFilters =
                            model.termFilters
                                |> List.map
                                    (\f ->
                                        if f.term.id == purlDateTimeISO then
                                            { f | value = newDateVal }
                                        else
                                            f
                                    )
                    in
                    ({ model
                        | doSearch = newDate /= Nothing
                        , termFilters = termFilters
                        , startDatePickers = Dict.insert id newDatePicker model.startDatePickers
                    }
                    , Cmd.none)

                Nothing -> -- should never happen
                    ( model, Cmd.none )

        SetEndDatePicker id val subMsg ->
            let
                endDatePicker =
                    Dict.get id model.endDatePickers
            in
            case endDatePicker of
                Just datePicker ->
                    let
                        ( newDatePicker, dateEvent ) =
                            DatePicker.update endDatePickerSettings subMsg datePicker

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

                        termFilters =
                            model.termFilters
                                |> List.map
                                    (\f ->
                                        if f.term.id == purlDateTimeISO then
                                            { f | value = newDateVal }
                                        else
                                            f
                                    )
                    in
                    ({ model
                        | doSearch = newDate /= Nothing
                        , termFilters = termFilters
                        , endDatePickers = Dict.insert id newDatePicker model.endDatePickers
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


defined : String -> Bool
defined s =
    s /= ""


validParam : FilterValue -> Bool
validParam val =
   case val of
        RangeValue min max ->
            defined min || defined max -- Either/both can defined --TODO check for valid number

        OffsetValue value ofs ->
            defined value && defined ofs --TODO check for valid number

        SearchValue s ->
            defined s

        SingleValue s ->
            defined s --True --TODO check for valid number

        MultipleValues vals ->
            List.all defined vals --TODO check for valid numbers

        DateTimeValue dt ->
            defined dt --TODO check for valid date format

        DateTimeRangeValue dt1 dt2 ->
            defined dt1 || defined dt2 --TODO check for valid date format

        NoValue ->
            True


encodeFilterValue : FilterValue -> String
encodeFilterValue val =
    let
        range from to =
            "[" ++ from ++ "," ++ to ++ "]"

        offset val2 ofs =
            val2 ++ "," ++ ofs
    in
    case val of
        RangeValue min max ->
            range min max

        OffsetValue value ofs ->
            offset value ofs --FIXME

        SearchValue s ->
            "~" ++ s

        SingleValue s ->
            s

        MultipleValues values ->
            String.join "|" values

        DateTimeValue dt ->
            dt

        DateTimeRangeValue dt1 dt2 ->
            range dt1 dt2

        NoValue ->
            ""


validLocationParam : LocationFilterValue -> Bool
validLocationParam val =
    case val of
        LatLngRadiusValue (lat,lng) radius ->
            defined lat && defined lng

        LonghurstValue s ->
            defined s

        NoLocationValue ->
            True


encodeLocationParam : LocationFilterValue -> String
encodeLocationParam val =
    case val of
        LatLngRadiusValue (lat,lng) radius ->
            let
                r =
                    if radius == "" then
                        "0"
                    else
                        radius
            in
            "[" ++ lat ++ "," ++ lng ++ "," ++ r ++ "]"

        LonghurstValue s ->
            s

        NoLocationValue ->
            ""


-- TODO refactor/simplify
generateQueryParams : LocationFilterValue -> Filter -> List Filter -> List Filter -> Result String (List (String, String))
generateQueryParams locationVal projectFilter fileFilters termFilters =
    --if locationVal == NoLocationValue && projectFilter.value == NoValue && Dict.isEmpty vals then
    --    Ok []
    --else
        let
            termParams =
                termFilters
                    |> List.map (\f -> Tuple.pair f.term.id (encodeFilterValue f.value))

            locParam =
                if validLocationParam locationVal then
                    [ ("location", encodeLocationParam locationVal) ]
                else
                    []

            depthParam =
                case List.filter (\f -> f.term.id == purlDepth) termFilters of
                    [ filter ] ->
                        if validParam filter.value then
                            [ ( purlDepth, encodeFilterValue filter.value )
                            --, ( "|" ++ purlDepthMin, encodeFilterValue term.value )
                            --, ( "|" ++ purlDepthMax, encodeFilterValue term.value )
                            ]
                        else
                            []

                    _ ->
                        []

            datetimeParam =
                case List.filter (\f -> f.term.id == purlDateTimeISO) termFilters of
                    [ filter ] ->
                        if validParam filter.value then
                            let
                                encodedVal =
                                    encodeFilterValue filter.value
                            in
                            [ ( "|" ++ purlDateTimeISO, encodedVal )
                            , ( "|" ++ purlDateTimeISOStart, encodedVal )
                            , ( "|" ++ purlDateTimeISOEnd, encodedVal )
                            ]
                        else
                            []

                    _ ->
                        []

            projectParam =
                if projectFilter.value /= NoValue then
                    [ ("project", encodeFilterValue projectFilter.value) ]
                else
                    []

            fileParams =
                fileFilters
                    |> List.filter (\f -> f.value /= NoValue)
                    |> List.map (\f -> Tuple.pair f.term.id (encodeFilterValue f.value))
        in
        List.concat [ projectParam, fileParams, locParam, depthParam, datetimeParam, termParams ]
            |> Ok
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


searchRequest : List (String, String) -> Http.Request SearchResponse
searchRequest queryParams =
    let
        url =
            apiBaseUrl ++ "/search"
    in
    HttpBuilder.get url
        |> HttpBuilder.withQueryParams queryParams
        |> HttpBuilder.withExpect (Http.expectJson decodeSearchResponse)
        |> HttpBuilder.toRequest


searchDownloadRequest : List (String, String) -> Http.Request String
searchDownloadRequest queryParams =
    let
        url =
            apiBaseUrl ++ "/search/download"
    in
    HttpBuilder.get url
        |> HttpBuilder.withQueryParams queryParams
        |> HttpBuilder.withExpect Http.expectString
        |> HttpBuilder.toRequest


type alias SearchResponse =
    { count : Int
    , results : SearchResults
    , summary : List Distribution --List SummaryResult
    , map : Value --List MapResult
    , error : Maybe String
    }


type SearchResults
    = SampleSearchResults (List SampleResult)
    | FileSearchResults (List FileResult)


type alias SampleResult =
    { schemaId : Int
    , sampleId : Int
    , sampleAccn : String
    , projectId : Int
    , projectName : String
    , values : List SearchResultValues
    }


type SearchResultValues
    = NoResultValues
    | SingleResultValue SearchResultValue
    | MultipleResultValues (List SearchResultValue)


type SearchResultValue
    = NoResultValue
    | NumberResultValue Float
    | StringResultValue String


type alias FileResult =
    { fileId : Int
    , fileUrl : String
    , fileFormat : String
    , fileType : String
    , sampleId : Int
    , sampleAccn : String
    , projectId : Int
    , projectName : String
    }


--type alias MapResult =
--    { centroid : String
--    , circle : String
--    , collection : String
--    , radius : Float
--    , count : Int
--    }


--type alias SummaryResult =
--    { projectId : Int
--    , projectName : String
--    , sampleCount : Int
--    }


type alias Sample =
    { sampleName : String
    , projectName : String
    , location : Location
    , depth : Float 
    , date : String
    }


type alias Location =
    { type_ : String
    , coordinates : List Float
    }


type FilterValue
    = NoValue
    | SingleValue String -- numeric/string value
    | RangeValue String String -- numeric min/max
    | OffsetValue String String -- numeric +/-
    | SearchValue String -- string value for partial match
    | MultipleValues (List String) -- multiple string literal values
    | DateTimeValue DatePicker --String -- single datetime value
    | DateTimeRangeValue DatePicker DatePicker --String String -- start/end datetime values


type LocationFilterValue
    = NoLocationValue
    | LatLngRadiusValue (String, String) String -- latitude/longitude with radius
    | LonghurstValue String -- Longhurst province


decodeSearchResponse : Decoder SearchResponse
decodeSearchResponse =
    Decode.succeed SearchResponse
        |> required "count" Decode.int
        |> required "results" decodeSearchResults
        |> required "summary" (Decode.list SearchTerm.distributionDecoder)
        |> optional "map" Decode.value null --(Decode.list decodeMapResult) []
        |> optional "error" (Decode.nullable Decode.string) Nothing


decodeSearchResults : Decoder SearchResults
decodeSearchResults =
    Decode.oneOf
        [ Decode.map SampleSearchResults (Decode.list decodeSampleResult)
        , Decode.map FileSearchResults (Decode.list decodeFileResult)
        ]


decodeSampleResult : Decoder SampleResult
decodeSampleResult =
    Decode.succeed SampleResult
        |> required "schemaId" Decode.int
        |> required "sampleId" Decode.int
        |> required "sampleAccn" Decode.string
        |> required "projectId" Decode.int
        |> required "projectName" Decode.string
        |> required "values" (Decode.list decodeSearchResultValues)


decodeSearchResultValues : Decoder SearchResultValues
decodeSearchResultValues =
    Decode.oneOf
        [ Decode.map SingleResultValue decodeSearchResultValue
        , Decode.map MultipleResultValues (Decode.list decodeSearchResultValue)
        , Decode.map (\a -> NoResultValues) (Decode.null a)
        ]


decodeSearchResultValue : Decoder SearchResultValue
decodeSearchResultValue =
    Decode.oneOf
        [ Decode.map NumberResultValue Decode.float
        , Decode.map StringResultValue Decode.string
        , Decode.map (\a -> NoResultValue) (Decode.null a)
        ]


decodeFileResult : Decoder FileResult
decodeFileResult =
    Decode.succeed FileResult
        |> required "fileId" Decode.int
        |> optional "fileUrl" Decode.string ""
        |> optional "fileFormat" Decode.string ""
        |> optional "fileType" Decode.string ""
        |> required "sampleId" Decode.int
        |> required "sampleAccn" Decode.string
        |> required "projectId" Decode.int
        |> required "projectName" Decode.string


--decodeMapResult : Decoder MapResult
--decodeMapResult =
--    Decode.succeed MapResult
--        |> required "centroid" Decode.string
--        |> required "circle" Decode.string
--        |> required "collection" Decode.string
--        |> required "radius" Decode.float
--        |> required "count" Decode.int


--encodeMapResult : MapResult -> Encode.Value
--encodeMapResult result =
--    Encode.object
--        [ ("centroid", Encode.string result.centroid)
--        , ("circle", Encode.string result.circle)
--        , ("collection", Encode.string result.collection)
--        , ("radius", Encode.float result.radius)
--        , ("count", Encode.int result.count)
--        ]


--decodeSummaryResult : Decoder SummaryResult
--decodeSummaryResult =
--    Decode.succeed SummaryResult
--        |> required "project_id" Decode.int
--        |> required "name" Decode.string
--        |> required "count" Decode.int


decodeSample : Decoder Sample
decodeSample =
    Decode.succeed Sample
        |> required "sample" Decode.string
        |> required "project" Decode.string
        |> required "location" decodeLocation
        |> required "depth" Decode.float
        |> required "collected" Decode.string


decodeLocation : Decoder Location
decodeLocation =
    Decode.succeed Location
        |> required "type" Decode.string
        |> required "coordinates" (Decode.list Decode.float)



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
            viewStringFilterDialog model.projectFilter

        ProjectSummaryDialog ->
            viewSearchTermSummaryDialog model.projectFilter.term

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
            viewFileSearchPanel model
          else
            text ""
        ]


viewSampleSearchPanel : Model -> Html Msg
viewSampleSearchPanel model =
    div []
        [ view4DPanel model
        , viewStringFilterPanel model.projectFilter
        , viewAddedFiltersPanel model model.selectedParams model.termFilters
        , viewAddFilterPanel model.showParamSearchDropdown model.paramSearchInputVal model.allParams model.selectedParams
        ]


viewFileSearchPanel : Model -> Html Msg
viewFileSearchPanel model =
    div []
        (List.map viewStringFilterPanel model.fileFilters)


viewTab : String -> Bool -> (String -> Msg) -> Html Msg
viewTab label isSelected msg =
    li [ class "nav-item" ]
        [ a [ class "nav-link", classList [ ("active", isSelected), ("font-weight-bold", isSelected) ], href "", style "color" "black", onClick (msg label) ]
            [ text label ]
        ]


view4DPanel : Model -> Html Msg
view4DPanel model =
    let
        depthVal =
            model.termFilters
                |> List.filter (\f -> f.term.id == purlDepth)
                |> List.head
                |> Maybe.map .value
                |> Maybe.withDefault NoValue

        datetimeFilter =
            model.termFilters
                |> List.filter (\f -> f.term.id == purlDateTimeISO)
                |> List.head

        startDatePicker =
            Dict.get purlDateTimeISO model.startDatePickers

        endDatePicker =
            Dict.get purlDateTimeISO model.endDatePickers
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
                                :: (viewLocationFilterInput model.locationVal)
                                ++ [ div [ class "input-group-append" ]
                                    [ viewFormatButton
                                    , div [ class "dropdown-menu" ]
                                        [ a [ class "dropdown-item active", href "", onClick (SetLocationFilterValue (LatLngRadiusValue ("","") "")) ] [ text "Lat, Lng (deg), Radius (km)" ]
                                        , a [ class "dropdown-item disabled", href "", disabled True, onClick (SetLocationFilterValue (LonghurstValue "")) ] [ text "Longhurst Province - coming soon" ]
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
                                (case (startDatePicker, endDatePicker, datetimeFilter) of
                                    (Just d1, Just d2, Just filter) ->
                                        List.concat
                                        [ viewDateTimeFilterInput d1 d2 filter
                                        , [ viewDateTimeFilterFormatOptions filter ]
                                        ]

                                    (_, _, _) -> -- error, shouldn't happen
                                        [ viewBlank ]
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


viewAddFilterPanel : Bool -> String -> List SearchTerm -> List PURL -> Html Msg
viewAddFilterPanel showDropdown searchVal allTerms selectedIDs =
    let
        makeOption term =
            let
                dis =
                    List.member term.id selectedIDs
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


viewAddedFiltersPanel : Model -> List PURL -> List Filter -> Html Msg
viewAddedFiltersPanel model selectedTerms filters  =
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
                                startDatePicker =
                                    Dict.get f.term.id model.startDatePickers

                                endDatePicker =
                                    Dict.get f.term.id model.endDatePickers
                            in
                            viewDateTimeFilterPanel startDatePicker endDatePicker f

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
    --let
    --    sortByName a b =
    --        let
    --            -- Translate purl to label (for Biome and Env Material terms)
    --            purlToLabel s =
    --                Dict.get s term.purlLabels |> Maybe.withDefault s
    --        in
    --        case compare (String.toLower (Tuple.first b |> purlToLabel)) (String.toLower (Tuple.first a |> purlToLabel)) of
    --            GT -> LT
    --            EQ -> EQ
    --            LT -> GT
    --
    --    options =
    --        term.distribution |> List.sortWith sortByName
    --in
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


viewDateTimeFilterPanel : DatePicker -> DatePicker -> Filter -> Html Msg
viewDateTimeFilterPanel startDatePicker endDatePicker filter =
    viewTermPanel filter.term
        [ div [ class "input-group input-group-sm" ]
            (List.append (viewDateTimeFilterInput startDatePicker endDatePicker filter)
                [ viewDateTimeFilterFormatOptions filter
                ]
            )
        ]


viewDateTimeFilterInput : DatePicker -> DatePicker -> Filter -> List (Html Msg)
viewDateTimeFilterInput startDatePicker endDatePicker filter =
    let
        singleInput dt =
            let
                date =
                    Date.fromIsoString dt |> Result.toMaybe
            in
            [ DatePicker.view
                date
                defaultDatePickerSettings
                startDatePicker
                |> Html.map (SetStartDatePicker filter.term.id filter.value)
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
                startDatePicker
                |> Html.map (SetStartDatePicker filter.term.id filter.value)
            , DatePicker.view
                date2
                endDatePickerSettings
                endDatePicker
                |> Html.map (SetEndDatePicker filter.term.id filter.value)
            ]
    in
    case filter.value of
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


viewDateTimeFilterFormatOptions : Filter -> Html Msg
viewDateTimeFilterFormatOptions filter =
    let
        viewOption (label, filterVal) =
            let
                isSelected =
                    case (filter.value, filterVal) of --FIXME kludgey
                        (DateTimeValue _, DateTimeValue _) ->
                            True

                        (DateTimeRangeValue _ _, DateTimeRangeValue _ _) ->
                            True

                        _ ->
                            False
            in
            a [ class "dropdown-item", classList [ ( "active", isSelected ) ], href "", onClick (SetFilterValue filter.term.id filterVal) ]
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


viewLocationFilterInput : LocationFilterValue -> List (Html Msg)
viewLocationFilterInput val =
    let
        latLngRadiusInput lat lng radius =
            [ input [ type_ "text", class "form-control", placeholder "lat", value lat, onInput (\p -> SetLocationFilterValue (LatLngRadiusValue (p,lng) radius)) ] []
            , input [ type_ "text", class "form-control", placeholder "lng", value lng, onInput (\p -> SetLocationFilterValue (LatLngRadiusValue (lat,p) radius)) ] []
            , input [ type_ "text", class "form-control", placeholder "radius", value radius, onInput (\p -> SetLocationFilterValue (LatLngRadiusValue (lat,lng) p)) ] []
            ]
    in
    case val of
        LatLngRadiusValue (lat,lng) radius ->
            latLngRadiusInput lat lng radius

        LonghurstValue s ->
            [ input [ type_ "text", class "form-control", placeholder "Longhurst province", value s, onInput (SetLocationFilterValue << LonghurstValue) ] []
            ]

        NoLocationValue ->
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
        isInitializing = -- page loading
            model.sampleResults == Nothing && model.fileResults == Nothing

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
        [ if (model.doSearch || model.isSearching) && model.errorMsg == Nothing then
            -- spinnner overlay
            div [ style "position" "fixed"
                , style "width" "100%"
                , style "height" "100%"
                , style "top" "0"
                , style "left" "0"
                , style "right" "0"
                , style "bottom" "0"
                , style "z-index" "2"
                , style "background-color" "rgba(0,0,0,0.3)"
                ]
                [ div [ style "padding-top" "40vh" ] [ viewSpinner ] ]
          else
            text ""
        , if model.errorMsg /= Nothing then
            div [ class "alert alert-danger m-3" ]
                [ p [] [ text "An error occurred:" ]
                , p [] [ text (model.errorMsg |> Maybe.withDefault "") ]
                ]
          else if isInitializing then
            text ""
          else if count == 0 then
            h1 [ class "text-center mt-5", style "min-height" "5.5em" ] [ text "No results" ]
          else
            div [ style "border" "1px solid lightgray" ]
                [ ul [ class "nav nav-tabs", style "width" "100%" ]
                    ((List.map (\lbl -> viewTab lbl (lbl == model.resultTab) SetResultTab) [ "Summary", "Samples", "Files" ] )
--                     ++ [ li [ class "nav-item ml-auto" ]
--                            [ a [ class "small nav-link", href "", style "font-weight" "bold" ] [ text "Columns" ] ]
--                        ]
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
                    --model.selectedParams
                    --    |> List.filterMap (\id -> Dict.get id model.termFilters |> Maybe.map .label)
                    model.termFilters |> List.map (.term >> .label)
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

        depthVal =
            model.termFilters
                |> List.filter (\f -> f.term.id == purlDepth)
                |> List.head
                |> Maybe.map .value
                |> Maybe.withDefault NoValue

        datetimeVal =
            model.termFilters
                |> List.filter (\f -> f.term.id == purlDateTimeISO)
                |> List.head
                |> Maybe.map .value
                |> Maybe.withDefault NoValue

        timeSpaceParamNames = -- kinda kludgey, find a better way to order time/space params
            List.concat
                [if model.locationVal /= NoLocationValue && validLocationParam model.locationVal then
                    [ "Location" ]
                  else
                    []
                , if depthVal /= NoValue && validParam depthVal then
                    [ "Depth" ] --, "Min Depth", "Max Depth" ]
                  else
                    []
                , if datetimeVal /= NoValue && validParam datetimeVal then
                    [ "Date", "Start Date", "End Date" ]
                  else
                    []
                |> List.filter (\s -> defined s)
                ]

        paramNames =
            List.concat
                [ [ "Project Name"
                  , "Sample ID"
                  ]
                , timeSpaceParamNames
                , (model.termFilters
                        |> List.map
                            (\f ->
                                if f.term.unitLabel /= "" then
                                    f.term.label ++ " (" ++ f.term.unitLabel ++ ")"
                                else
                                    f.term.label
                            )

                    )
                ]

        addToCartTh =
            th [ class "text-right", style "min-width" "10em" ]
                [ Cart.addAllToCartButton (Session.getCart model.session) Nothing
                    (model.sampleResults
                        |> Maybe.withDefault []
                        |> List.map .sampleId
                    )
                    |> Html.map CartMsg
                ]

        --aliases =
        --    model.selectedParams
        --        |> List.filterMap
        --            (\param ->
        --                case Dict.get param model.selectedTerms of
        --                    Nothing ->
        --                        Nothing
        --
        --                    Just term ->
        --                        Just term.aliases
        --            )

        --mkTh2 a =
        --    if List.length a > 1 then
        --        th [] [ text (List.map .name a |> String.join ", ") ]
        --    else
        --        th [] []

        columns =
            [ tr [] (List.indexedMap mkTh paramNames ++ [ addToCartTh ])
--            , tr [] ((List.repeat ((List.length timeSpaceParamNames) + 2) (th [] [])) ++ (List.map mkTh2 aliases)) --FIXME kludgey
            ]

        mkTd label =
            td [ style "max-width" maxColWidth ] [ text label ]

        formatVals vals =
            case vals of
                SingleResultValue v ->
                    [ v ]

                MultipleResultValues l ->
                    l

                NoResultValues ->
                    []

        formatVal val =
            case val of
                StringResultValue s ->
                    s

--                MultipleStringResultValue l ->
--                    String.join ", " l

                NumberResultValue n ->
                    String.fromFloat n

--                MultipleNumberResultValue l ->
--                    l |> List.map String.fromFloat |> String.join ", "

                NoResultValue ->
                    ""

        mkRow result =
            tr []
                (List.concat --FIXME kludgey
                    [ [ td [] [ a [ Route.href (Route.Project result.projectId) ] [ text result.projectName ] ] ]
                    , [ td [] [ a [ Route.href (Route.Sample result.sampleId) ] [ text result.sampleAccn ] ] ]
                    , result.values
                        |> List.map formatVals
                        |> List.map (\a -> List.map (formatVal) a |> String.join ", " |> mkTd)
                    , [ td [ class "text-right", style "min-width" "10em" ]
                        [ Cart.addToCartButton (Session.getCart model.session) result.sampleId |> Html.map CartMsg ]
                      ]
                    ]
                )
    in
    SortableTable.view
        { tableAttrs = [ class "table table-sm table-striped", style "font-size" "0.85em" ] }
        model.sampleTableState
        columns
        (model.sampleResults |> Maybe.withDefault [] |> List.map mkRow)
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
