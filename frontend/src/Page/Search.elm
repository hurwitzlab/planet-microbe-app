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
import Page exposing (viewBlank, viewSpinner, viewDialog)
import Sample exposing (SearchTerm, PURL)
import File exposing (FileFormat, FileType)
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
    3


minNumPanelOptionsForSearchBar =
    100


purlDepth =
    "http://purl.obolibrary.org/obo/ENVO_3100031"


purlDepthMin =
    "http://purl.obolibrary.org/obo/PMO_00000172"


purlDepthMax =
    "http://purl.obolibrary.org/obo/PMO_00000052"


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


initialParams =
    [ purlBiome
    ]



-- MODEL --


type alias Model =
    { session : Session
    , projectCounts : List ProjectCount
    , fileFormats : List FileFormat
    , fileTypes : List FileType
    , allParams : List SearchTerm -- list of available params to add
    --TODO combine selectedParams/Terms/Vals into list of records, use get and set routines to access by PURL
    , selectedParams : List PURL -- added params, for maintaining order
    , selectedTerms : Dict PURL SearchTerm
    , selectedVals : Dict PURL FilterValue
    , locationVal : LocationFilterValue
    , projectVals : List String
    , fileFormatVals : List String
    , fileTypeVals : List String
    , showAddFilterDialog : Bool
    , stringFilterDialogTerm : Maybe SearchTerm
    , chartFilterDialogTerm : Maybe SearchTerm
    , paramSearchInputVal : String
    , dialogSearchInputVal : String
    , showParamSearchDropdown : Bool
    , doSearch : Bool
    , searchStartTime : Int -- milliseconds
    , isSearching : Bool
    , sampleResults : Maybe (List SampleResult)
    , fileResults : Maybe (List FileResult)
    , mapResults : Value --List MapResult
    , summaryResults : Maybe (List (List (String, Int)))
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
    , startDatePickers : Dict PURL DatePicker
    , endDatePickers : Dict PURL DatePicker
    , previousSearchParams : List (String, String)
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        ( startDatePicker, _ ) =
            DatePicker.init

        ( endDatePicker, _ ) =
            DatePicker.init
    in
    (
        { session = session
        , projectCounts = []
        , fileFormats = []
        , fileTypes = []
        , allParams = []
        , selectedParams = initialParams
        , selectedTerms = Dict.empty
        , selectedVals = Dict.empty
        , locationVal = NoLocationValue
        , projectVals = []
        , fileFormatVals = []
        , fileTypeVals = []
        , showAddFilterDialog = False
        , stringFilterDialogTerm = Nothing
        , chartFilterDialogTerm = Nothing
        , paramSearchInputVal = ""
        , dialogSearchInputVal = ""
        , showParamSearchDropdown = False
        , doSearch = True
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
        , startDatePickers = Dict.insert purlDateTimeISO startDatePicker Dict.empty
        , endDatePickers = Dict.insert purlDateTimeISO endDatePicker Dict.empty
        , previousSearchParams = []
        }
    , Cmd.batch
        [ Sample.fetchSearchTerms |> Http.toTask |> Task.attempt GetAllSearchTermsCompleted
        , initialParams |> List.map Sample.fetchSearchTerm |> List.map Http.toTask |> List.map (Task.attempt GetSearchTermCompleted) |> Cmd.batch
        , getProjectCounts |> Http.toTask |> Task.attempt GetProjectCountsCompleted
        , File.fetchFormats |> Http.toTask |> Task.attempt GetFileFormatsCompleted
        , File.fetchTypes |> Http.toTask |> Task.attempt GetFileTypesCompleted
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
    = GetProjectCountsCompleted (Result Http.Error (List ProjectCount))
    | GetFileFormatsCompleted (Result Http.Error (List FileFormat))
    | GetFileTypesCompleted (Result Http.Error (List FileType))
    | GetAllSearchTermsCompleted (Result Http.Error (List SearchTerm))
    | GetSearchTermCompleted (Result Http.Error SearchTerm)
    | ClearFilters
    | AddFilter PURL
    | RemoveFilter PURL
    | OpenFilterChartDialog PURL
    | CloseFilterChartDialog
    | SetParamSearchInput String
    | ShowParamSearchDropdown
    | OpenAddFilterDialog
    | CloseAddFilterDialog
    | SetDialogSearchInput String
    | OpenStringFilterDialog SearchTerm
    | CloseStringFilterDialog
    | SetSearchFilterValue PURL String
    | SetStringFilterValue PURL String Bool
    | SetFilterValue PURL FilterValue
    | SetLocationFilterValue LocationFilterValue
    | SetProjectFilterValue String Bool
    | SetFileFormatFilterValue String Bool
    | SetFileTypeFilterValue String Bool
    | SetSearchTab String
    | SetResultTab String
    | SetSampleSortPos Int
    | SetFileSortPos Int
    | SetPageSize Int
    | SetPageNum Int
    | InputTimerTick Time.Posix
    | Search Int
    | SampleSearchCompleted (Result Http.Error SearchResponse)
    | FileSearchCompleted (Result Http.Error SearchResponse)
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
            ( { model | projectCounts = counts }, Cmd.none )


        GetProjectCountsCompleted (Err error) -> --TODO
--            let
--                _ = Debug.log "GetProjectCountsCompleted" (toString error)
--            in
            ( model, Cmd.none )

        GetFileFormatsCompleted (Ok formats) ->
            ( { model | fileFormats = formats }, Cmd.none )


        GetFileFormatsCompleted (Err error) -> --TODO
--            let
--                _ = Debug.log "GetFileFormatsCompleted" (toString error)
--            in
            ( model, Cmd.none )

        GetFileTypesCompleted (Ok types) ->
            ( { model | fileTypes = types }, Cmd.none )


        GetFileTypesCompleted (Err error) -> --TODO
--            let
--                _ = Debug.log "GetFileTypesCompleted" (toString error)
--            in
            ( model, Cmd.none )

        GetAllSearchTermsCompleted (Ok terms) ->
            ( { model | allParams = terms }, Cmd.none )

        GetAllSearchTermsCompleted (Err error) -> --TODO
--            let
--                _ = Debug.log "GetAllSearchTermsCompleted" (toString error)
--            in
            ( model, Cmd.none )

        GetSearchTermCompleted (Ok term) ->
            let
                selectedParams =
                    List.singleton term.id |> List.append model.selectedParams |> List.Extra.unique -- cannot use Set because it doesn't preserve order

                selectedTerms =
                    Dict.insert term.id term model.selectedTerms

                val =
                    case term.type_ of
                        "number" ->
                            RangeValue (String.fromFloat term.min) (String.fromFloat term.max)

                        _ ->
                            NoValue

                selectedVals =
                    Dict.insert term.id val model.selectedVals
            in
            ( { model | doSearch = True, selectedParams = selectedParams, selectedTerms = selectedTerms, selectedVals = selectedVals }, Cmd.none )

        GetSearchTermCompleted (Err error) -> --TODO
--            let
--                _ = Debug.log "GetSearchTermCompleted" (toString error)
--            in
            ( model, Cmd.none )

        ClearFilters ->
            let
                newVals =
                    model.selectedVals
                        |> Dict.remove purlDepth
                        |> Dict.remove purlDateTimeISO
                        |> Dict.map (\k v -> NoValue)
            in
            ( { model
                | doSearch = True
                , locationVal = NoLocationValue
                , selectedParams = initialParams
                , selectedVals = newVals
                , projectVals = []
                , fileFormatVals = []
                , fileTypeVals = []
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
            ( { model | showParamSearchDropdown = False, paramSearchInputVal = "", showAddFilterDialog = False }, Task.attempt GetSearchTermCompleted getTerm )

        RemoveFilter id ->
            let
                newParams =
                    model.selectedParams |> List.filter (\n -> n /= id) -- cannot use Set because it doesn't preserve order

                newTerms =
                    Dict.remove id model.selectedTerms

                newVals =
                    Dict.remove id model.selectedVals
            in
            ( { model
                | doSearch = True
                , selectedParams = newParams
                , selectedTerms = newTerms
                , selectedVals = newVals
                , sampleTableState = SortableTable.initialState
                , fileTableState = SortableTable.initialState
              }
            , Cmd.none
            )

        OpenFilterChartDialog id ->
            let
                maybeTerm =
                    Dict.get id model.selectedTerms
            in
            ( { model | chartFilterDialogTerm = maybeTerm }, Cmd.none )

        CloseFilterChartDialog ->
            ( { model | chartFilterDialogTerm = Nothing }, Cmd.none )

        SetParamSearchInput val ->
            ( { model | paramSearchInputVal = val }, Cmd.none )

        ShowParamSearchDropdown ->
            ( { model | showParamSearchDropdown = not model.showParamSearchDropdown }, Cmd.none )

        OpenAddFilterDialog ->
            ( { model | showAddFilterDialog = True, dialogSearchInputVal = "" }, Cmd.none )

        CloseAddFilterDialog ->
            ( { model | showAddFilterDialog = False }, Cmd.none )

        SetDialogSearchInput val ->
            ( { model | dialogSearchInputVal = val }, Cmd.none )

        OpenStringFilterDialog term ->
            ( { model | stringFilterDialogTerm = Just term }, Cmd.none )

        CloseStringFilterDialog ->
            ( { model | stringFilterDialogTerm = Nothing }, Cmd.none )

        SetSearchFilterValue id val ->
            let
                doSearch =
                    val == "" || String.length val > 2

                newVal =
                    if val == "" then
                        NoValue
                    else
                        SearchValue val

                newVals =
                    Dict.insert id newVal model.selectedVals
            in
            ( { model | doSearch = doSearch, selectedVals = newVals }, Cmd.none )

        SetStringFilterValue id val enable ->
            let
--                _ = Debug.log "SetStringFilterValue" (toString (id, val, enable))

                newVal =
                    case Dict.get id model.selectedVals of
                        Nothing -> -- Error
                            NoValue

                        Just termVal ->
                            case termVal of
                                NoValue ->
                                    if enable then
                                        SingleValue val
                                    else
                                        NoValue

                                SingleValue val1 -> --FIXME merge into MultipleValues case?
                                    if enable then
                                        MultipleValues [val1, val]
                                    else
                                        NoValue

                                MultipleValues vals ->
                                    vals
                                        |> Set.fromList
                                        |> (if enable then Set.insert val else Set.remove val)
                                        |> Set.toList
                                        |> MultipleValues

                                _ -> -- error
                                    NoValue

                newVals =
                    Dict.insert id newVal model.selectedVals
            in
            ( { model | doSearch = True, selectedVals = newVals }, Cmd.none )

        SetFilterValue id val ->
            let
                newVals =
                    Dict.insert id val model.selectedVals
            in
            ( { model | doSearch = True, selectedVals = newVals }, Cmd.none )

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

        SetProjectFilterValue val enable ->
            let
                vals =
                    model.projectVals
                        |> Set.fromList
                        |> (if enable then Set.insert val else Set.remove val)
                        |> Set.toList
            in
            ( { model | doSearch = True, projectVals = vals }, Cmd.none )

        SetFileFormatFilterValue val enable ->
            let
                vals =
                    model.fileFormatVals
                        |> Set.fromList
                        |> (if enable then Set.insert val else Set.remove val)
                        |> Set.toList
            in
            ( { model | doSearch = True, fileFormatVals = vals }, Cmd.none )

        SetFileTypeFilterValue val enable ->
            let
                vals =
                    model.fileTypeVals
                        |> Set.fromList
                        |> (if enable then Set.insert val else Set.remove val)
                        |> Set.toList
            in
            ( { model | doSearch = True, fileTypeVals = vals }, Cmd.none )

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
                update (Search newPageNum) model
            else
                ( model, Cmd.none )

        InputTimerTick time ->
            if model.doSearch && Time.posixToMillis time - model.searchStartTime >= 1000 then -- 1 second
                update (Search 0) model
            else
                ( model, Cmd.none )

        Search newPageNum ->
            case generateQueryParams model.locationVal model.projectVals model.fileFormatVals model.fileTypeVals model.selectedParams model.selectedVals of
                Ok queryParams ->
                    let
                        ( result, sortPos, cmd ) =
                            if model.resultTab == "Files" then
                                ( "file", model.fileTableState.sortCol * (SortableTable.directionToInt model.fileTableState.sortDir), FileSearchCompleted )
                            else
                                ( "sample", model.sampleTableState.sortCol * (SortableTable.directionToInt model.sampleTableState.sortDir), SampleSearchCompleted )

                        allParams =
                            ("summary", String.join "," model.selectedParams) ::
                            queryParams ++
                            (generateControlParams result [] sortPos model.pageSize (model.pageSize * newPageNum) model.showMap)

                        searchReq =
                            searchRequest allParams
                    in
                    if allParams == model.previousSearchParams then
                        ( model, Cmd.none )
                    else
                    ( { model
                        | doSearch = False
                        , isSearching = True
                        , pageNum = newPageNum
                        , previousSearchParams = allParams
                      }
                    , searchReq |> Http.toTask |> Task.attempt cmd
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
                , isSearching = False
              }
            , GMap.loadMap response.map
            )

        SampleSearchCompleted (Err error) ->
--            let
--                _ = Debug.log "SampleSearchCompleted" (toString error)
--            in
            ( { model | errorMsg = Just "Error", isSearching = False }, Cmd.none ) --TODO

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
--            let
--                _ = Debug.log "FileSearchCompleted" (toString error)
--            in
            ( { model | errorMsg = Just "Error", isSearching = False }, Cmd.none ) --TODO

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
                    in
                    ({ model
                        | doSearch = newDate /= Nothing
                        , selectedVals = Dict.insert id newDateVal model.selectedVals
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
                    in
                    ({ model
                        | doSearch = newDate /= Nothing
                        , selectedVals = Dict.insert id newDateVal model.selectedVals
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


formatParam : FilterValue -> String
formatParam val = --TODO use encoder instead
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


formatLocationParam : LocationFilterValue -> String
formatLocationParam val = --TODO use encoder instead
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
generateQueryParams : LocationFilterValue -> List String -> List String -> List String -> List PURL -> Dict PURL FilterValue -> Result String (List (String, String))
generateQueryParams locationVal projectVals fileFormatVals fileTypeVals params vals =
    let
        list values =
            String.join "|" values
    in
    --FIXME refactor section below
    if locationVal == NoLocationValue && projectVals == [] && Dict.isEmpty vals then
        Ok []
    else --else if vals |> Dict.toList |> List.map Tuple.second |> List.all validParam then
        let
            userParams =
                params -- maintain filter order
                    |> List.map (\id ->
                        (id, (Dict.get id vals |> Maybe.withDefault NoValue))
                    )
                    |> List.map (Tuple.mapSecond formatParam)

            locParam =
                if validLocationParam locationVal then
                    [ ("location", formatLocationParam locationVal) ]
                else
                    []

            depthParam =
                case Dict.get purlDepth vals of
                    Nothing ->
                        []

                    Just val ->
                        if validParam val then
                            let
                                fmtVal =
                                    formatParam val
                            in
                            [ ( "|" ++ purlDepth, fmtVal )
                            , ( "|" ++ purlDepthMin, fmtVal )
                            , ( "|" ++ purlDepthMax, fmtVal )
                            ]
                        else
                            []

            datetimeParam =
                case Dict.get purlDateTimeISO vals of
                    Nothing ->
                        []

                    Just val ->
                        if validParam val then
                            let
                                fmtVal =
                                    formatParam val
                            in
                            [ ( "|" ++ purlDateTimeISO, fmtVal )
                            , ( "|" ++ purlDateTimeISOStart, fmtVal )
                            , ( "|" ++ purlDateTimeISOEnd, fmtVal )
                            ]
                        else
                            []

            projectParam =
                if projectVals /= [] then
                    [ ("project", list projectVals) ]
                else
                    []

            fileFormatParam =
                if fileFormatVals /= [] then
                    [ ("fileFormat", list fileFormatVals) ]
                else
                    []

            fileTypeParam =
                if fileTypeVals /= [] then
                    [ ("fileType", list fileTypeVals) ]
                else
                    []
        in
        List.concat [ projectParam, fileFormatParam, fileTypeParam, locParam, depthParam, datetimeParam, userParams ]
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


getProjectCounts : Http.Request (List ProjectCount)
getProjectCounts =
    let
        url =
            apiBaseUrl ++ "/projects"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list decodeProjectCount))
        |> HttpBuilder.toRequest


type alias SearchResponse =
    { count : Int
    , results : SearchResults
    , summary : List (List (String, Int)) --List SummaryResult
    , map: Value --List MapResult
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
    | DateTimeValue String -- single datetime value
    | DateTimeRangeValue String String -- start/end datetime values


type LocationFilterValue
    = NoLocationValue
    | LatLngRadiusValue (String, String) String -- latitude/longitude with radius
    | LonghurstValue String -- Longhurst province


type alias ProjectCount  =
    { name : String
    , sampleCount : Int
    }


decodeSearchResponse : Decoder SearchResponse
decodeSearchResponse =
    Decode.succeed SearchResponse
        |> required "count" Decode.int
        |> required "results" decodeSearchResults
        |> required "summary" (Decode.list (Decode.list (Decode.map2 Tuple.pair (Decode.index 0 Decode.string) (Decode.index 1 Decode.int)))) --(Decode.list decodeSummaryResult)
        |> optional "map" Decode.value null --(Decode.list decodeMapResult) []


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


decodeProjectCount : Decoder ProjectCount
decodeProjectCount =
    Decode.succeed ProjectCount
        |> required "name" Decode.string
        |> required "sample_count" Decode.int



-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "container-fluid" ]
        [ br [] []
        , div [ class "float-left", style "width" "26%" ]
            [ viewSearchPanel model ]
        , div [ class "float-right", style "width" "74%", style "padding-left" "1em" ]
--            [ viewSearchSummary model
--            , br [] []
            [ viewMap model.showMap model.mapLoaded
            , viewResults model
            ]
        , if model.showAddFilterDialog then
            viewAddFilterDialog model.allParams model.dialogSearchInputVal
          else
            text ""
        , case model.stringFilterDialogTerm of
            Nothing ->
                viewBlank

            Just term ->
                viewStringFilterDialog term (Dict.get term.id model.selectedVals |> Maybe.withDefault NoValue)
        , case model.chartFilterDialogTerm of
            Nothing ->
                viewBlank

            Just term ->
                viewSearchTermSummaryDialog term
        ]


viewSearchPanel : Model -> Html Msg
viewSearchPanel model =
    div [ style "border" "1px solid lightgray", style "width" "25.5vw" ]
        [ ul [ class "nav nav-tabs" ]
            ( (List.map (\lbl -> viewTab lbl (lbl == model.searchTab) SetSearchTab) [ "Samples" ]) ++ --, "Files" ]) ++
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
    let
        projectCounts =
            model.projectCounts |> List.map (\pc -> (pc.name, pc.sampleCount))
    in
    div []
        [ view4DPanel model
        , viewProjectPanel projectCounts model.projectVals
        , viewAddedFiltersPanel model model.selectedParams model.selectedTerms model.selectedVals
        , viewAddFilterPanel model.showParamSearchDropdown model.paramSearchInputVal model.allParams model.selectedParams
        ]


viewFileSearchPanel : Model -> Html Msg
viewFileSearchPanel model =
    let
        formatCounts =
            model.fileFormats |> List.map (\ff -> (ff.name, ff.fileCount))

        typeCounts =
            model.fileTypes |> List.map (\ft -> (ft.name, ft.fileCount))
    in
    div []
        [ viewFileFormatPanel formatCounts model.fileFormatVals
        , viewFileTypePanel typeCounts model.fileTypeVals
        ]


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
            Dict.get purlDepth model.selectedVals |> Maybe.withDefault NoValue

        datetimeVal =
            Dict.get purlDateTimeISO model.selectedVals |> Maybe.withDefault (DateTimeRangeValue "" "")
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
                                        [ a [ class "dropdown-item active", href "", onClick (SetLocationFilterValue (LatLngRadiusValue ("","") "")) ] [ text "Lat, Lng (deg), Radius (m)" ]
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
                            ((div [ class "input-group-prepend" ] [ span [ class "input-group-text", style "width" "5em" ] [ text "Date"] ])
                                :: (viewDateTimeFilterInput model purlDateTimeISO datetimeVal)
                                ++ [ viewDateTimeFilterFormatOptions purlDateTimeISO datetimeVal ]
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
            String.contains (String.toLower searchVal) (String.toLower term.label)

        options =
            allTerms
                |> List.filter removeRedundantTerms
                |> List.filter filterOnSearch
                |> List.sortWith (\a b -> compare (String.Extra.toSentenceCase a.label) (String.Extra.toSentenceCase b.label) )
                |> List.map makeOption

        show =
            searchVal /= "" || showDropdown
    in
    viewPanel "" "Add Filter" "" False False
        [ div [ class "input-group input-group-sm", style "position" "relative" ]
            [ input [ type_ "text", class "form-control", placeholder "Search parameters", value searchVal, onInput SetParamSearchInput ] []
            , div [ class "input-group-append" ]
                [ button [ class "btn btn-outline-secondary", type_ "button", onClick OpenAddFilterDialog ] [ text "?" ]
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
                            , td [] [ text (noneIfBlank term.unitLabel) ]
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
        [ button [ type_ "button", class "btn btn-secondary", onClick CloseAddFilterDialog ] [ text "Close" ] ]
        CloseAddFilterDialog


viewAddedFiltersPanel : Model -> List PURL -> Dict PURL SearchTerm -> Dict PURL FilterValue -> Html Msg
viewAddedFiltersPanel model params terms vals  =
    div []
        (params
            |> List.map
                (\param ->
                    case Dict.get param terms of
                        Nothing ->
                            viewBlank

                        Just term ->
                            let
                                termVal =
                                    Dict.get param vals |> Maybe.withDefault NoValue
                            in
                            case term.type_ of
                                "string" ->
                                    if List.length term.distribution >= minNumPanelOptionsForSearchBar then
                                        viewSearchFilterPanel term termVal
                                    else
                                        viewStringFilterPanel term termVal

                                "number" ->
                                    viewNumberFilterPanel term termVal

                                "datetime" ->
                                    viewDateTimeFilterPanel model term termVal

                                _ ->
                                    text "Error"
                )
        )


viewSearchFilterPanel : SearchTerm -> FilterValue -> Html Msg
viewSearchFilterPanel term val =
    let
        numValues =
            List.length term.distribution

        val2 =
            case val of
                SearchValue s ->
                    s

                _ ->
                    ""
    in
    viewTermPanel term
        [ label [] [ numValues |> toFloat |> format myLocale |> text, text " unique values" ]
        , div [ class "input-group input-group-sm" ]
            [ input [ type_ "text", class "form-control", placeholder "Search ...", value val2, onInput (SetSearchFilterValue term.id) ] [] ]
        ]


viewProjectPanel : List (String, Int) -> List String -> Html Msg --TODO merge with viewFileFormatPanel/viewStringFilterPanel
viewProjectPanel counts selectedVals =
    let
        viewRow vals ( lbl, num ) =
            let
                isChecked =
                    List.member lbl vals
            in
            div []
                [ div [ class "form-check form-check-inline" ]
                    [ input [ class "form-check-input", type_ "checkbox", checked isChecked, onCheck (SetProjectFilterValue lbl) ] []
                    , label [ class "form-check-label" ] [ text lbl ]
                    ]
                , div [ class "badge badge-secondary float-right" ] [ num |> toFloat |> format myLocale |> text ]
                ]

        truncatedOptions =
            counts |> List.sortBy Tuple.second |> List.reverse --|> List.take 4 maxNumPanelOptions

        numOptions =
            List.length counts

        numMore =
            0 --numOptions - maxNumPanelOptions
    in
    viewPanel "" "Project" "" False True
        [ div [] (List.map (viewRow selectedVals) truncatedOptions)
        , if numMore > 0 then
            button [ class "btn btn-sm btn-link float-right" ] [ String.fromInt numMore ++ " More ..." |> text ]
          else
            viewBlank
        ]


viewFileFormatPanel : List (String, Int) -> List String -> Html Msg --TODO merge with viewProjectPanel/viewStringFilterPanel
viewFileFormatPanel counts selectedVals =
    let
        viewRow vals ( lbl, num ) =
            let
                isChecked =
                    List.member lbl vals
            in
            div []
                [ div [ class "form-check form-check-inline" ]
                    [ input [ class "form-check-input", type_ "checkbox", checked isChecked, onCheck (SetFileFormatFilterValue lbl) ] []
                    , label [ class "form-check-label" ] [ text (String.Extra.toSentenceCase lbl) ]
                    ]
                , div [ class "badge badge-secondary float-right" ]
                    [ num |> toFloat |> format myLocale |> text ]
                ]

        truncatedOptions =
            counts |> List.sortBy Tuple.second --|> List.take 4 maxNumPanelOptions

        numOptions =
            List.length counts

        numMore =
            numOptions - maxNumPanelOptions
    in
    viewPanel "" "Format" "" False False
        [ div [] (List.map (viewRow selectedVals) truncatedOptions)
        , if numMore > 0 then
            button [ class "btn btn-sm btn-link float-right" ] [ String.fromInt numMore ++ " More ..." |> text ]
          else
            viewBlank
        ]


viewFileTypePanel : List (String, Int) -> List String -> Html Msg --TODO merge with viewFileFormatPanel/viewStringFilterPanel
viewFileTypePanel counts selectedVals =
    let
        viewRow vals ( lbl, num ) =
            let
                isChecked =
                    List.member lbl vals
            in
            div []
                [ div [ class "form-check form-check-inline" ]
                    [ input [ class "form-check-input", type_ "checkbox", checked isChecked, onCheck (SetFileTypeFilterValue lbl) ] []
                    , label [ class "form-check-label" ] [ text (String.Extra.toSentenceCase lbl) ]
                    ]
                , div [ class "badge badge-secondary float-right" ]
                    [ num |> toFloat |> format myLocale |> text ]
                ]

        truncatedOptions =
            counts |> List.sortBy Tuple.second --|> List.take 4 maxNumPanelOptions

        numOptions =
            List.length counts

        numMore =
            numOptions - maxNumPanelOptions
    in
    viewPanel "" "Type" "" False False
        [ div [] (List.map (viewRow selectedVals) truncatedOptions)
        , if numMore > 0 then
            button [ class "btn btn-sm btn-link float-right" ] [ String.fromInt numMore ++ " More ..." |> text ]
          else
            viewBlank
        ]


viewStringFilterPanel : SearchTerm -> FilterValue -> Html Msg
viewStringFilterPanel term val =
    let
        numOptions =
            List.length term.distribution

        numSelected =
            term.distribution |> List.filter (\a -> isStringFilterSelected (Tuple.first a) val) |> List.length

        sortByCount a b =
            case compare (Tuple.second a) (Tuple.second b) of
                LT -> GT
                EQ -> EQ
                GT -> LT

        sortBySelected a b =
            case ( isStringFilterSelected (Tuple.first a) val, isStringFilterSelected (Tuple.first b) val ) of
                (True, False) ->
                    LT

                (False, True) ->
                    GT

                (_, _) ->
                    sortByCount a b

        truncatedOptions =
            term.distribution |> List.sortWith sortBySelected |> List.take (Basics.max maxNumPanelOptions numSelected)
    in
    viewTermPanel term
        [ div []
            (viewStringFilterOptions term val truncatedOptions)
        , if numOptions > maxNumPanelOptions then
            button [ class "btn btn-sm btn-link float-right", onClick (OpenStringFilterDialog term) ]
                [ String.fromInt (numOptions - maxNumPanelOptions) ++ " More ..." |> text ]
          else
            viewBlank
        ]


viewStringFilterOptions : SearchTerm -> FilterValue -> List (String, Int) -> List (Html Msg)
viewStringFilterOptions term val options =
    let
        viewRow (name, count) =
            -- Using table layout to fix issue with wrapping rows
            table [ style "width" "100%" ]
                [ tr []
                    [ td []
                        [ div [ class "form-check form-check-inline" ]
                            [ input [ class "form-check-input", type_ "checkbox", checked (isStringFilterSelected name val), onCheck (SetStringFilterValue term.id name) ] []
                            , label [ class "form-check-label" ] [ name |> String.Extra.toSentenceCase |> text]
                            ]
                        ]
                    , td [ style "max-width" "3em" ] [ div [ class "badge badge-secondary float-right align-top" ] [ count |> toFloat |> format myLocale |> text ] ]
                    ]
                ]
    in
    List.map viewRow options


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


viewStringFilterDialog : SearchTerm -> FilterValue -> Html Msg
viewStringFilterDialog term val =
    let
        sortByName a b =
            case compare (String.toLower (Tuple.first b)) (String.toLower (Tuple.first a)) of
                GT -> LT
                EQ -> EQ
                LT -> GT

        options =
            term.distribution --|> List.sortWith sortByName
    in
    viewDialog (String.Extra.toTitleCase term.label)
        [ div [ style "overflow-y" "auto", style "max-height" "50vh" ] (viewStringFilterOptions term val options) ]
        [ button [ type_ "button", class "btn btn-secondary", onClick CloseStringFilterDialog ] [ text "Close" ] ]
        CloseStringFilterDialog


viewNumberFilterPanel : SearchTerm -> FilterValue -> Html Msg
viewNumberFilterPanel term val =
    viewTermPanel term
        [ div [ class "input-group input-group-sm" ]
            (List.append (viewNumberFilterInput term.id val)
                [ viewNumberFilterFormatOptions term.id val
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


viewDateTimeFilterPanel : Model -> SearchTerm -> FilterValue -> Html Msg
viewDateTimeFilterPanel model term val =
    viewTermPanel term
        [ div [ class "input-group input-group-sm" ]
            (List.append (viewDateTimeFilterInput model term.id val)
                [ viewDateTimeFilterFormatOptions term.id val
                ]
            )
        ]


viewDateTimeFilterInput : Model -> PURL -> FilterValue -> List (Html Msg)
viewDateTimeFilterInput model id val =
    let
--        singleInput dt =
--            [ input [ type_ "text", class "form-control", placeholder "value", value dt, onInput (\p -> SetFilterValue id (DateTimeValue p)) ] [] ]
--
--        rangeInput dt1 dt2 =
--            [ input [ type_ "text", class "form-control", placeholder "start", value dt1, onInput (\p -> SetFilterValue id (DateTimeRangeValue p dt2)) ] []
--            , input [ type_ "text", class "form-control", placeholder "end", value dt2, onInput (\p -> SetFilterValue id (DateTimeRangeValue dt1 p)) ] []
--            ]

        startDatePicker =
            Dict.get id model.startDatePickers

        endDatePicker =
            Dict.get id model.endDatePickers

        singleInput dt =
            let
                date =
                    Date.fromIsoString dt |> Result.toMaybe
            in
            case startDatePicker of
                Just datePicker ->
                    [ DatePicker.view
                        date
                        defaultDatePickerSettings
                        datePicker
                     |> Html.map (SetStartDatePicker id val)
                    ]

                Nothing -> -- should never happen
                    [ text "error" ]

        rangeInput dt1 dt2 =
            let
                date1 =
                    Date.fromIsoString dt1 |> Result.toMaybe

                date2 =
                    Date.fromIsoString dt2 |> Result.toMaybe
            in
            case (startDatePicker, endDatePicker) of
                (Just datePicker1, Just datePicker2) ->
                    [ DatePicker.view
                        date1
                        startDatePickerSettings
                        datePicker1
                        |> Html.map (SetStartDatePicker id val)
                    , DatePicker.view
                        date2
                        endDatePickerSettings
                        datePicker2
                        |> Html.map (SetEndDatePicker id val)
                    ]

                (_, _) -> -- should never happen
                    [ text "error" ]
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
--            [ a [ class "dropdown-item", href "#" ] [ text "Time YY-MM-DD HH:MM:SS" ]
--            , a [ class "dropdown-item", href "#" ] [ text "Day YY-MM-DD" ]
--            , a [ class "dropdown-item", href "#" ] [ text "Year YYYY" ]
--            ]
            [ ("Point (YYYY-MM-DD)", DateTimeValue "")
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
    viewPanel term.id term.label term.unitLabel True True nodes


viewPanel : PURL -> String -> String -> Bool -> Bool -> List (Html Msg) -> Html Msg
viewPanel id title unit removable showChart nodes =
    let
        header =
            h6 [ style "color" "darkblue"]
                [ text (String.fromChar (Char.fromCode 9660))
                , text " "
                , text (String.Extra.toTitleCase title)
                , text " "
                , if unit /= "" then
                    small [ style "margin-left" "5px" ] [ text ("[" ++ unit ++ "]") ]
                  else
                    viewBlank
                , if removable then
                    span [ class "float-right ml-2", style "cursor" "pointer", onClick (RemoveFilter id) ]
                        [ text (String.fromChar (Char.fromCode 10005)) ]
                  else
                    viewBlank
                , if showChart then
                    span [ class "float-right", style "cursor" "pointer", onClick (OpenFilterChartDialog id) ]
                        [ Icon.barChart ]
                  else
                    viewBlank
                ]
    in
    div [ class "card", style "font-size" "0.85em" ]
        [ div [ class "card-body" ]
            (header :: nodes)
        ]


--viewSearchSummary : Model -> Html Msg
--viewSearchSummary model =
--    let
--        format param =
--            case Dict.get param model.selectedVals |> Maybe.withDefault Nothing of
--                Nothing ->
--                    "<error>"
--
--                Just (StringValue s) ->
--                    param ++ " = " ++ s
--
--                Just (MultipleStringValues list) ->
--                    String.join "," list
--
--                Just (NumberValue (min, max)) ->
--                    param ++ " between [" ++ (String.fromInt min) ++ "," ++ (String.fromInt max) ++ "]"
--
--        searchStr =
--            model.selectedParams |> List.map format |> String.join " AND "
--
--        content =
--            if model.selectedParams == [] then
--                div []
--                    [ text "Begin by selecting filters on the left or try the "
--                    , a [ class "alert-link", href "#" ] [ text "Advanced Search" ]
--                    ]
--            else
--                text searchStr
--    in
--    div [ class "card" ]
--        [ div [ class "card-body" ]
--            [ content ]
--        ]


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
        [ if model.doSearch || model.isSearching then
            viewSpinner
          else
            div [ style "border" "1px solid lightgray" ]
                [ ul [ class "nav nav-tabs", style "width" "100%" ]
                    ((List.map (\lbl -> viewTab lbl (lbl == model.resultTab) SetResultTab) [ "Summary", "Samples" ] ) --, "Files" ] )
--                     ++ [ li [ class "nav-item ml-auto" ]
--                            [ a [ class "small nav-link", href "", style "font-weight" "bold" ] [ text "Columns" ] ]
--                        ]
                    )
                , if model.errorMsg /= Nothing then
                    div []
                        [ p [] [ text "An error occurred:" ]
                        , p [] [ text (model.errorMsg |> Maybe.withDefault "") ]
                        ]
                  else if count > 0 then
                    div []
                        [ viewPageSummary model.pageNum model.pageSize count
                        , content
                        , viewPageControls model.pageNum model.pageSize count
                        ]
                  else
                    h1 [ class "text-center mt-5", style "min-height" "5.5em" ] [ text "No results" ]
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
                    model.selectedParams
                        |> List.filterMap (\id -> Dict.get id model.selectedTerms |> Maybe.map .label)
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

        config =
            { defaultBarChartConfig
                | title = Just ("Num Samples by " ++ (String.Extra.toTitleCase label))
                , width = 400
                , height = 400
            }
    in
    BarChart.view config (List.map (Tuple.mapSecond toFloat) data)


viewSearchTermSummaryDialog : SearchTerm -> Html Msg
viewSearchTermSummaryDialog term =
    viewDialog (String.Extra.toTitleCase term.label)
        [ div [ style "overflow-y" "auto", style "max-height" "50vh", style "text-align" "center", style "margin-top" "2em" ]
            [ viewSearchTermSummaryChart term.label term.distribution ]
        ]
        [ button [ type_ "button", class "btn btn-secondary", onClick CloseFilterChartDialog ]
            [ text "Close" ]
        ]
        CloseFilterChartDialog


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
            Dict.get purlDepth model.selectedVals |> Maybe.withDefault NoValue

        datetimeVal =
            Dict.get purlDateTimeISO model.selectedVals |> Maybe.withDefault NoValue

        timeSpaceParamNames = -- kinda kludgey, find a better way to order time/space params
            List.concat
                [if model.locationVal /= NoLocationValue && validLocationParam model.locationVal then
                    [ "Location" ]
                  else
                    []
                , if depthVal /= NoValue && validParam depthVal then
                    [ "Depth", "Min Depth", "Max Depth" ]
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
                , (model.selectedParams
                        |> List.filterMap
                            (\param ->
                                case Dict.get param model.selectedTerms of
                                    Nothing ->
                                        Nothing

                                    Just term ->
                                        if term.unitLabel /= "" then
                                            Just (term.label ++ " (" ++ term.unitLabel ++ ")")
                                        else
                                            Just term.label
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

        aliases =
            model.selectedParams
                |> List.filterMap
                    (\param ->
                        case Dict.get param model.selectedTerms of
                            Nothing ->
                                Nothing

                            Just term ->
                                Just term.aliases
                    )

        mkTh2 a =
            if List.length a > 1 then
                th [] [ text (List.map .name a |> String.join ", ") ]
            else
                th [] []

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
                    , result.values |> List.map formatVals |> List.map (\a -> List.map (formatVal) a |> String.join ", " |> mkTd)
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
