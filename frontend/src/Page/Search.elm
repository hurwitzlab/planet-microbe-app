module Page.Search exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onCheck)
import Http
import HttpBuilder
import Json.Encode as Encode exposing (Value, null)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
--import Date exposing (Date, day, month, weekday, year)
--import DatePicker exposing (DateEvent(..), defaultSettings)
import Time exposing (Weekday(..))
import Task
import String.Extra
import List.Extra
import Set
import GMap
import Dict exposing (Dict)
import Route
import Page exposing (viewBlank, viewSpinner, viewDialog)
import Sample exposing (SearchTerm, PURL, annotationsToHide)
import File exposing (FileFormat, FileType)
import SortableTable
import Cart
import Icon
--import Debug exposing (toString)
import Config exposing (apiBaseUrl, dataCommonsUrl)



myLocale =
    { usLocale | decimals = 0 }


defaultPageSize =
    20


maxNumPanelOptions =
    3


minNumPanelOptionsForSearchBar =
    100


purlSampleID =
    "http://purl.obolibrary.org/obo/OBI_0001901"


purlBiome =
    "http://purl.obolibrary.org/obo/ENVO_00000428"


purlDepth =
    "http://purl.obolibrary.org/obo/ENVO_3100031" --"http://purl.obolibrary.org/obo/PMO_00000029"


purlDateTime =
    "http://purl.obolibrary.org/obo/PMO_00000008"


facets =
    [ ("Biomes", purlBiome)
    ]


initialParams =
    [ purlBiome
    ]



-- MODEL


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
    , paramSearchInputVal : String
    , dialogSearchInputVal : String
    , showParamSearchDropdown : Bool
    , doSearch : Bool
    , searchStartTime : Int -- milliseconds
    , isSearching : Bool
    , sampleResults : Maybe (List SampleResult)
    , fileResults : Maybe (List FileResult)
    , mapResults : Value --List MapResult
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
    }


-- lat/lon (constrained to -180/180, -90/90, respectively), date, depth.
init : Session -> ( Model, Cmd Msg )
init session =
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
        , paramSearchInputVal = ""
        , dialogSearchInputVal = ""
        , showParamSearchDropdown = False
        , doSearch = True
        , searchStartTime = 0
        , isSearching = True
        , sampleResults = Nothing
        , fileResults = Nothing
        , mapResults = Encode.object []
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
subscriptions model =
    Sub.batch
        [ Time.every 1000 InputTimerTick -- milliseconds
        , GMap.getLocation UpdateLocationFromMap
        , GMap.mapLoaded MapLoaded
        ]


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE


type Msg
    = GetProjectCountsCompleted (Result Http.Error (List ProjectCount))
    | GetFileFormatsCompleted (Result Http.Error (List FileFormat))
    | GetFileTypesCompleted (Result Http.Error (List FileType))
    | GetAllSearchTermsCompleted (Result Http.Error (List SearchTerm))
    | GetSearchTermCompleted (Result Http.Error SearchTerm)
    | ClearFilters
    | AddFilter PURL
    | RemoveFilter PURL
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
            ( { model | doSearch = True, selectedTerms = selectedTerms, selectedVals = selectedVals }, Cmd.none )

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
                    |> Dict.remove purlDateTime
                    |> Dict.map (\k v -> NoValue)
            in
            ( { model
                | doSearch = True
                , locationVal = NoLocationValue
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
                params =
                    List.singleton id |> List.append model.selectedParams |> List.Extra.unique -- cannot use Set because it doesn't preserve order

                getTerm =
                    Sample.fetchSearchTerm id |> Http.toTask
            in
            ( { model | showParamSearchDropdown = False, paramSearchInputVal = "", selectedParams = params, showAddFilterDialog = False }, Task.attempt GetSearchTermCompleted getTerm )

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
--                _ = Debug.log "SetFilterValue" (toString (id, val))

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
                            if model.resultTab == "Samples" then
                                ( "sample", model.sampleTableState.sortCol * (SortableTable.directionToInt model.sampleTableState.sortDir), SampleSearchCompleted )
                            else
                                ( "file", model.fileTableState.sortCol * (SortableTable.directionToInt model.fileTableState.sortDir), FileSearchCompleted )

                        searchTask =
                            searchRequest queryParams result sortPos model.pageSize (model.pageSize * newPageNum) model.showMap
                                |> Http.toTask
                                |> Task.attempt cmd
                    in
                    ( { model
                        | doSearch = False
                        , isSearching = True
                        , pageNum = newPageNum
                      }
                    , searchTask
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
            defined value && defined ofs -- TODO check for valid number

        SearchValue s ->
            defined s

        SingleValue s ->
            True

        MultipleValues vals ->
            List.all defined vals

        DateTimeValue dt ->
            defined dt --TODO

        DateTimeRangeValue dt1 dt2 ->
            defined dt1 && defined dt2 --TODO

        NoValue ->
            True


validLocationParam : LocationFilterValue -> Bool
validLocationParam val =
    case val of
--        LatLngValue lat lng ->
--            defined lat && defined lng
--
--        LatLngRangeValue (lat1,lng1) (lat2,lng2) ->
--            defined lat1 && defined lng1 && defined lat2 && defined lng2

        LatLngRadiusValue (lat,lng) radius ->
            defined lat && defined lng

        LonghurstValue s ->
            defined s

        NoLocationValue ->
            True


-- TODO refactor/simplify
generateQueryParams : LocationFilterValue -> List String -> List String -> List String -> List PURL -> Dict PURL FilterValue -> Result String (List (String, String))
generateQueryParams locationVal projectVals fileFormatVals fileTypeVals params vals =
    let
        range from to =
            "[" ++ from ++ "," ++ to ++ "]"

        offset val ofs =
            val ++ "," ++ ofs

        list values =
            String.join "|" values

        formatParam val = --TODO use encoder instead
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

        formatLocationParam val = --TODO use encoder instead
            case val of
--                LatLngValue lat lng ->
--                    range lat lng
--
--                LatLngRangeValue (lat1,lng1) (lat2,lng2) ->
--                    "[" ++ lat1 ++ "," ++ lng1 ++ "-" ++ lat2 ++ "," ++ lng2 ++ "]"

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
    in
    --FIXME refactor section below
    if locationVal == NoLocationValue && projectVals == [] && Dict.isEmpty vals then
        Ok [(purlSampleID, "")]
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
                        [ (purlDepth, if validParam val then formatParam val else "") ]

            datetimeParam =
                case Dict.get purlDateTime vals of
                    Nothing ->
                        []

                    Just val ->
                        [ (purlDateTime, if validParam val then formatParam val else "") ]

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
        List.concat [ projectParam, fileFormatParam, fileTypeParam, [(purlSampleID, "")], locParam, depthParam, datetimeParam, userParams ]
            |> Ok
--    else
--        Err "Invalid query parameter"


searchRequest : List (String, String) -> String -> Int -> Int -> Int -> Bool -> Http.Request SearchResponse
searchRequest queryParams result sortPos limit offset showMap =
    let
        url =
            apiBaseUrl ++ "/search"

        queryParams2 =
            queryParams
                |> List.append [ ("result", result) ]
                |> List.append [ ("sort", String.fromInt sortPos) ]
                |> List.append [ ("limit", String.fromInt limit) ]
                |> List.append [ ("offset", String.fromInt offset) ]
                |> List.append [ ("map", if showMap then "1" else "0") ]
    in
    HttpBuilder.get url
        |> HttpBuilder.withQueryParams queryParams2
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
    , map: Value --List MapResult
    }


type SearchResults
    = SampleSearchResults (List SampleResult)
    | FileSearchResults (List FileResult)


type alias SampleResult =
    { schemaId : Int
    , sampleId : Int
    , projectId : Int
    , projectName : String
    , values : List SearchResultValue
    }


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


type SearchResultValue
    = NoResultValue
    | NumberResultValue Float
    | StringResultValue String


--type alias MapResult =
--    { centroid : String
--    , circle : String
--    , collection : String
--    , radius : Float
--    , count : Int
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
--    | LatLngValue String String -- latitude/longitude
--    | LatLngRangeValue (String, String) (String, String) -- latitude/longitude to latitude/longitude
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
        |> required "projectId" Decode.int
        |> required "projectName" Decode.string
        |> required "values" (Decode.list decodeSearchResultValue)


decodeFileResult : Decoder FileResult
decodeFileResult =
    Decode.succeed FileResult
        |> optional "fileId" Decode.int 0 -- optional for case where file table not yet loaded
        |> optional "fileUrl" Decode.string ""
        |> optional "fileFormat" Decode.string ""
        |> optional "fileType" Decode.string ""
        |> required "sampleId" Decode.int
        |> required "sampleAccn" Decode.string
        |> required "projectId" Decode.int
        |> required "projectName" Decode.string


decodeSearchResultValue : Decoder SearchResultValue
decodeSearchResultValue =
    Decode.oneOf
        [ Decode.map NumberResultValue Decode.float
        , Decode.map StringResultValue Decode.string
        , Decode.map (\a -> NoResultValue) (Decode.null a)
        ]


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



-- VIEW


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
                text ""

            Just term ->
                viewStringFilterDialog term (Dict.get term.id model.selectedVals |> Maybe.withDefault NoValue)
        ]


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
    let
        projectCounts =
            model.projectCounts |> List.map (\pc -> (pc.name, pc.sampleCount))
    in
    div []
        [ viewLocationPanel model
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


viewLocationPanel : Model -> Html Msg
viewLocationPanel model =
    let
        depthVal =
            Dict.get purlDepth model.selectedVals |> Maybe.withDefault NoValue

        datetimeVal =
            Dict.get purlDateTime model.selectedVals |> Maybe.withDefault (DateTimeRangeValue "" "")
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
                            ((div [ class "input-group-prepend" ] [ span [ class "input-group-text", style "width" "6em" ] [ text "Location"] ])
                                :: (viewLocationFilterInput model.locationVal)
                                ++ [ div [ class "input-group-append" ]
                                    [ viewFormatButton
                                    , div [ class "dropdown-menu" ]
                                        [ a [ class "dropdown-item active", href "", onClick (SetLocationFilterValue (LatLngRadiusValue ("","") "")) ] [ text "Lat, Lng (deg), Radius (m)" ]
--                                        , a [ class "dropdown-item", href "", onClick (SetLocationFilterValue (  ("","") ("",""))) ] [ text "Lat min/max, Lng min/max (deg)" ]
--                                        , a [ class "dropdown-item", href "", onClick (SetLocationFilterValue (LatLngValue "" "")) ] [ text "Lat, Lng (deg)" ]
                                        , a [ class "dropdown-item disabled", href "", disabled True, onClick (SetLocationFilterValue (LonghurstValue "")) ] [ text "Longhurst Province - coming soon" ]
                                        ]
                                    ]
                                ]
                            )
                        ]
                    , br [] []
                    , div [ class "form-row" ]
                        [ div [ class "input-group input-group-sm" ]
                            ((div [ class "input-group-prepend" ] [ span [ class "input-group-text", style "width" "6em" ] [ text "Depth"] ])
                                :: (viewNumberFilterInput purlDepth depthVal)
                                ++ [ viewNumberFilterFormatOptions purlDepth depthVal ]
                            )
                        ]
                    , br [] []
                    , div [ class "form-row" ]
                        [ div [ class "input-group input-group-sm" ]
                            ((div [ class "input-group-prepend" ] [ span [ class "input-group-text", style "width" "6em" ] [ text "Date/Time"] ])
                                :: (viewDateTimeFilterInput model purlDateTime datetimeVal)
                                ++ [ viewDateTimeFilterFormatOptions purlDateTime ]
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
    viewPanel "" "Add Filter" "" False
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
                                    if Dict.size term.values >= minNumPanelOptionsForSearchBar then
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
            Dict.size term.values

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
    viewPanel "" "Project" "" False
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
    viewPanel "" "Format" "" False
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
    viewPanel "" "Type" "" False
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
            Dict.size term.values

        numSelected =
            Dict.toList term.values |> List.filter (\a -> isStringFilterSelected (Tuple.first a) val) |> List.length

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
            Dict.toList term.values |> List.sortWith sortBySelected |> List.take (Basics.max maxNumPanelOptions numSelected)
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
            Dict.toList term.values |> List.sortWith sortByName
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
                [ viewDateTimeFilterFormatOptions term.id
                ]
            )
        ]


viewDateTimeFilterInput : Model -> PURL -> FilterValue -> List (Html Msg)
viewDateTimeFilterInput model id val =
    let
        singleInput dt =
            [ input [ type_ "text", class "form-control", placeholder "value", value dt, onInput (\p -> SetFilterValue id (DateTimeValue p)) ] [] ]
    in
    case val of
        DateTimeValue dt ->
            singleInput dt

        DateTimeRangeValue dt1 dt2 ->
            [ input [ type_ "text", class "form-control", placeholder "start", value dt1, onInput (\p -> SetFilterValue id (DateTimeRangeValue p dt2)) ] []
            , input [ type_ "text", class "form-control", placeholder "end", value dt2, onInput (\p -> SetFilterValue id (DateTimeRangeValue dt1 p)) ] []
            ]

        _ ->
            singleInput ""


viewDateTimeFilterFormatOptions : PURL -> Html Msg
viewDateTimeFilterFormatOptions id =
    let
        viewOption (label, filterVal) =
            a [ class "dropdown-item active", href "", onClick (SetFilterValue id filterVal) ]
                [ label |> String.Extra.toSentenceCase |> text ]

        options =
--            [ a [ class "dropdown-item", href "#" ] [ text "Time YY-MM-DD HH:MM:SS" ]
--            , a [ class "dropdown-item", href "#" ] [ text "Day YY-MM-DD" ]
--            , a [ class "dropdown-item", href "#" ] [ text "Year YYYY" ]
--            , a [ class "dropdown-item", href "#" ] [ text "Range YY-MM-DD HH:MM:SS, YY-MM-DD HH:MM:SS" ]
--            ]
            [ ("Range YY-MM-DD HH:MM:SS, YY-MM-DD HH:MM:SS", DateTimeRangeValue "" "")
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
--        LatLngValue lat lng ->
--            [ input [ type_ "text", class "form-control", placeholder "lat", value lat, onInput (\p -> SetLocationFilterValue (LatLngValue p lng)) ] []
--            , input [ type_ "text", class "form-control", placeholder "lng", value lng, onInput (\p -> SetLocationFilterValue (LatLngValue lat p)) ] []
--            ]

--        LatLngRangeValue (lat1,lng1) (lat2,lng2) ->
--            [ input [ type_ "text", class "form-control", placeholder "lat1", value lat1, onInput (\p -> SetLocationFilterValue (LatLngRangeValue (p,lng1) (lat2,lng2))) ] []
--            , input [ type_ "text", class "form-control", placeholder "lng1", value lng1, onInput (\p -> SetLocationFilterValue (LatLngRangeValue (lat1,p) (lat2,lng2))) ] []
--            , text " "
--            , input [ type_ "text", class "form-control", placeholder "lat2", value lat2, onInput (\p -> SetLocationFilterValue (LatLngRangeValue (lat1,lng2) (p,lng2))) ] []
--            , input [ type_ "text", class "form-control", placeholder "lng2", value lng2, onInput (\p -> SetLocationFilterValue (LatLngRangeValue (lat1,lng2) (lat2,p))) ] []
--            ]

        LatLngRadiusValue (lat,lng) radius ->
            latLngRadiusInput lat lng radius

        LonghurstValue s ->
            [ input [ type_ "text", class "form-control", placeholder "Longhurst province", value s, onInput (SetLocationFilterValue << LonghurstValue) ] []
            ]

        NoLocationValue ->
            latLngRadiusInput "" "" ""


viewTermPanel : SearchTerm -> List (Html Msg) -> Html Msg
viewTermPanel term nodes =
    viewPanel term.id term.label term.unitLabel True nodes


viewPanel : PURL -> String -> String -> Bool -> List (Html Msg) -> Html Msg
viewPanel id title unit removable nodes =
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
                    span [ class "float-right", style "cursor" "pointer", onClick (RemoveFilter id) ]
                        [ text (String.fromChar (Char.fromCode 10005)) ]
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


--TODO combine viewSampleResults/viewFileResults and pass columns as input
viewResults : Model -> Html Msg
viewResults model =
    if model.resultTab == "Samples" then
        viewSampleResults model
    else
        viewFileResults model


viewSampleResults : Model -> Html Msg
viewSampleResults model =
    let
        maxColWidth = "8em"

        mkTh index label =
            let
                pos =
                    index + 1

                dirSymbol =
                    if model.sampleTableState.sortDir == SortableTable.ASC then
                        String.fromChar (Char.fromCode 9650)
                    else
                        String.fromChar (Char.fromCode 9660)

                lbl =
                    String.Extra.toTitleCase
                        (if pos == abs model.sampleTableState.sortCol then
                            label ++ " " ++ dirSymbol
                        else
                            label
                        )
            in
            th [ style "cursor" "pointer", style "max-width" maxColWidth, onClick (SetSampleSortPos pos) ] [ text lbl ]

        timeSpaceParamNames = -- kinda kludgey, find another way to order time/space params
            let
                depthVal =
                    Dict.get purlDepth model.selectedVals |> Maybe.withDefault NoValue

                datetimeVal =
                    Dict.get purlDateTime model.selectedVals |> Maybe.withDefault NoValue
            in
            [ if model.locationVal /= NoLocationValue && validLocationParam model.locationVal then
                "Location"
              else
                ""
            , if depthVal /= NoValue then --&& validParam depthVal then
                "Depth"
              else
                ""
            , if datetimeVal /= NoValue then --&& validParam datetimeVal then
                "Date/Time"
              else
                ""
            ]
            |> List.filter (\s -> defined s)

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

        columns =
            List.indexedMap mkTh paramNames ++ [ addToCartTh ]

        mkTd label =
            td [ style "max-width" maxColWidth ] [ text label ]

        formatVal val =
            case val of
                StringResultValue s ->
                    s

                NumberResultValue n ->
                    String.fromFloat n

                NoResultValue ->
                    ""

        mkRow result =
            tr []
                (List.concat --FIXME kludgey
                    [ [ mkTd result.projectName ]
                    , [ td [] [ a [ Route.href (Route.Sample result.sampleId) ]
                        [ text (List.head result.values |> Maybe.withDefault NoResultValue |> formatVal) ] ]
                      ]
                    , result.values |> List.tail |> Maybe.withDefault [] |> List.map (formatVal >> mkTd)
                    , [ td [ class "text-right", style "min-width" "10em" ]
                        [ Cart.addToCartButton (Session.getCart model.session) result.sampleId |> Html.map CartMsg ]
                      ]
                    ])

        count =
            model.sampleResults |> Maybe.withDefault [] |> List.length

        pageInfo =
            div [ class "small ml-1", style "color" "dimgray" ]
                (if model.sampleResultCount > 0 then
                    [ text "Showing "
                    , model.pageNum * model.pageSize + 1 |> Basics.max 1 |> String.fromInt |> text
                    , text " - "
                    , model.pageNum * model.pageSize + model.pageSize |> Basics.max 1 |> Basics.min model.sampleResultCount |> String.fromInt |> text
                    , text " of "
                    , model.sampleResultCount |> toFloat |> format myLocale |> text
                    , text " sample"
                    , (if model.sampleResultCount /= 1 then "s" else "") |> text
                    ]
                 else
                    [ text "No results" ]
                 )

        lastPageNum =
            toFloat(model.sampleResultCount) / toFloat(model.pageSize) |> floor

        pageControls =
            let
                sizeOption size =
                    a [ class "dropdown-item", href "", onClick (SetPageSize size) ] [ text (String.fromInt size) ]

                pageOption label num =
                    let
                        dis =
                            num < 0 -- previous
                                || num == model.pageNum -- current
                                || num > lastPageNum -- next
                    in
                    li [ classList [ ("page-item", True), ("disabled", dis) ] ]
                        [ a [ class "page-link", href "", onClick (SetPageNum num) ] [ text label ] ]
            in
            div [ style "padding" "0.5em" ]
                [ div [ class "float-left" ]
                    [ text "Show "
                    , div [ class "dropup", style "display" "inline" ]
                        [ button [ class "btn btn-secondary dropdown-toggle", type_ "button", attribute "data-toggle" "dropdown" ] [ text (String.fromInt model.pageSize) ]
                        , div [ class "dropdown-menu" ]
                            (List.map sizeOption [20, 40, 60, 80, 100])
                        ]
                    , text " results"
                ]
                , nav [ class "float-right" ]
                    [ ul [ class "pagination" ]
                        --FIXME code below is a little kludgey
                        [ pageOption "First" 0
                        , pageOption "Previous" (model.pageNum - 1)
                        , pageOption (String.fromInt (model.pageNum + 1)) model.pageNum
                        , if model.pageNum + 1 > lastPageNum then text "" else pageOption (String.fromInt (model.pageNum + 2)) (model.pageNum + 1)
                        , if model.pageNum + 2 > lastPageNum then text "" else pageOption (String.fromInt (model.pageNum + 3)) (model.pageNum + 2)
                        , if model.pageNum + 3 > lastPageNum then text "" else pageOption "..." (model.pageNum + 3)
                        , pageOption "Next" (model.pageNum + 1)
                        , pageOption "Last" lastPageNum
                        ]
                    ]
                ]
    in
    if model.isSearching || model.sampleResults == Nothing then
        viewSpinner
    else
        div []
            [ div [ style "border" "1px solid lightgray" ]
                [ ul [ class "nav nav-tabs", style "width" "100%" ]
                    (List.map (\lbl -> viewTab lbl (lbl == model.resultTab) SetResultTab) [ "Samples", "Files" ] )
--                          , li [ class "nav-item ml-auto" ] --TODO
--                              [ a [ class "small nav-link", href "", style "font-weight" "bold" ] [ text "Columns" ] ]
                , pageInfo
                , if model.errorMsg /= Nothing then
                    div []
                        [ p [] [ text "An error occurred:" ]
                        , p [] [ text (model.errorMsg |> Maybe.withDefault "") ]
                        ]
                  else if model.sampleResultCount > 0 then
                    SortableTable.view
                        { tableAttrs = [ class "table table-sm table-striped", style "font-size" "0.85em" ] }
                        model.sampleTableState
                        columns
                        (model.sampleResults |> Maybe.withDefault [] |> List.map mkRow)
                        []
                  else
                    viewBlank
                ]
            , if count > 0 then
                pageControls
              else
                viewBlank
            ]


viewFileResults : Model -> Html Msg
viewFileResults model =
    let
        maxColWidth = "8em"

        mkTh index label =
            let
                pos =
                    index + 1

                dirSymbol =
                    if model.fileTableState.sortDir == SortableTable.ASC then
                        String.fromChar (Char.fromCode 9650)
                    else
                        String.fromChar (Char.fromCode 9660)

                lbl =
                    String.Extra.toTitleCase
                        (if pos == abs model.fileTableState.sortCol then
                            label ++ " " ++ dirSymbol
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

        count =
            model.fileResults |> Maybe.withDefault [] |> List.length

        pageInfo =
            div [ class "small ml-1", style "color" "dimgray" ]
                (if model.fileResultCount > 0 then
                    [ text "Showing "
                    , model.pageNum * model.pageSize + 1 |> Basics.max 1 |> String.fromInt |> text
                    , text " - "
                    , model.pageNum * model.pageSize + model.pageSize |> Basics.max 1 |> Basics.min model.fileResultCount |> String.fromInt |> text
                    , text " of "
                    , model.fileResultCount |> toFloat |> format myLocale |> text
                    , text " file"
                    , (if model.fileResultCount /= 1 then "s" else "") |> text
                    ]
                 else
                    [ text "No results" ]
                )

        lastPageNum =
            toFloat(model.fileResultCount) / toFloat(model.pageSize) |> floor

        pageControls =
            let
                sizeOption size =
                    a [ class "dropdown-item", href "", onClick (SetPageSize size) ] [ text (String.fromInt size) ]

                pageOption label num =
                    let
                        dis =
                            num < 0 -- previous
                                || num == model.pageNum -- current
                                || num > lastPageNum -- next
                    in
                    li [ classList [ ("page-item", True), ("disabled", dis) ] ]
                        [ a [ class "page-link", href "", onClick (SetPageNum num) ] [ text label ] ]
            in
            div [ style "padding" "0.5em" ]
                [ div [ class "float-left" ]
                    [ text "Show "
                    , div [ class "dropup", style "display" "inline" ]
                        [ button [ class "btn btn-secondary dropdown-toggle", type_ "button", attribute "data-toggle" "dropdown" ] [ text (String.fromInt model.pageSize) ]
                        , div [ class "dropdown-menu" ]
                            (List.map sizeOption [20, 40, 60, 80, 100])
                        ]
                    , text " results"
                ]
                , nav [ class "float-right" ]
                    [ ul [ class "pagination" ]
                        --FIXME code below is a little kludgey
                        [ pageOption "First" 0
                        , pageOption "Previous" (model.pageNum - 1)
                        , pageOption (String.fromInt (model.pageNum + 1)) model.pageNum
                        , if model.pageNum + 1 > lastPageNum then text "" else pageOption (String.fromInt (model.pageNum + 2)) (model.pageNum + 1)
                        , if model.pageNum + 2 > lastPageNum then text "" else pageOption (String.fromInt (model.pageNum + 3)) (model.pageNum + 2)
                        , if model.pageNum + 3 > lastPageNum then text "" else pageOption "..." (model.pageNum + 3)
                        , pageOption "Next" (model.pageNum + 1)
                        , pageOption "Last" lastPageNum
                        ]
                    ]
                ]
    in
    if model.isSearching || model.fileResults == Nothing then
        viewSpinner
    else
        div []
            [ div [ style "border" "1px solid lightgray" ]
                [ ul [ class "nav nav-tabs", style "width" "100%" ]
                    (List.map (\lbl -> viewTab lbl (lbl == model.resultTab) SetResultTab) [ "Samples", "Files" ] )
--                          , li [ class "nav-item ml-auto" ] --TODO
--                              [ a [ class "small nav-link", href "", style "font-weight" "bold" ] [ text "Columns" ] ]
                , pageInfo
                , if model.errorMsg /= Nothing then
                    div []
                        [ p [] [ text "An error occurred:" ]
                        , p [] [ text (model.errorMsg |> Maybe.withDefault "") ]
                        ]
                  else if model.fileResultCount > 0 then
--                    table [ class "table table-sm table-striped", style "font-size" "0.85em" ]
--                        [ thead []
--                            [ tr [] columns ]
--                        , tbody []
--                            (model.fileResults |> Maybe.withDefault [] |> List.map mkRow)
--                        ]
                    SortableTable.view
                        { tableAttrs = [ class "table table-sm table-striped", style "font-size" "0.85em" ] }
                        model.fileTableState
                        columns
                        (model.fileResults |> Maybe.withDefault [] |> List.map mkRow)
                        []
                  else
                    viewBlank
                ]
            , if count > 0 then
                pageControls
              else
                viewBlank
            ]


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
