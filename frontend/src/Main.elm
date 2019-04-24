import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onCheck)
import Http
import HttpBuilder
import Json.Encode as Encode exposing (Value, string)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Task
import Time
import String.Extra
import Set
import GMap
import Dict exposing (Dict)
import Debug exposing (toString)
import Config exposing (apiBaseUrl)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 InputTimerTick -- milliseconds
        ]


myLocale =
    { usLocale | decimals = 0 }


defaultPageSize =
    20


maxNumPanelOptions =
    4


minNumPanelOptionsForSearchBar =
    30


purlSampleID =
    "http://purl.obolibrary.org/obo/OBI_0001901"


purlBiome =
    "http://purl.obolibrary.org/obo/ENVO_00000428"


purlDepth =
    "http://purl.obolibrary.org/obo/PMO_00000029"


purlDateTime =
    "http://purl.obolibrary.org/obo/PMO_00000008"


facets =
    [ ("Biomes", purlBiome)
    ]


initialParams =
    [ purlBiome
    ]



-- MODEL


type alias PURL =
    String


type alias Model =
    { projectCounts : List ProjectCount
    , allParams : List SearchTerm -- list of available params to add
--    , availableParams : List String -- based on params already selected
    , selectedParams : List PURL -- added params, for maintaining order
    , selectedTerms : Dict PURL SearchTerm
    , selectedVals : Dict PURL FilterValue
    , locationVal : LocationFilterValue
    , projectVals : List String
    , stringFilterDialogTerm : Maybe SearchTerm
    , paramSearchInputVal : String
    , showParamSearchDropdown : Bool
    , sortPos : Int
    , doSearch : Bool
    , isSearching : Bool
    , searchStartTime : Int -- milliseconds
    , results : Maybe (List SearchResult)
    , count : Int
    , errorMsg : Maybe String
    , pageNum : Int
    , pageSize : Int
    , mapState : GMap.MapState
    , showMap : Bool
    }


-- lat/lon (constrained to -180/180, -90/90, respectively), date, depth.
init : Value -> ( Model, Cmd Msg )
init flags =
    (
        { projectCounts = []
        , allParams = []
--        , availableParams = []
        , selectedParams = initialParams
        , selectedTerms = Dict.empty
        , selectedVals = Dict.empty
        , locationVal = NoLocationValue
        , projectVals = []
        , stringFilterDialogTerm = Nothing
        , paramSearchInputVal = ""
        , showParamSearchDropdown = False
        , sortPos = 1
        , doSearch = False
        , isSearching = False
        , searchStartTime = 0
        , results = Nothing
        , count = 0
        , errorMsg = Nothing
        , pageNum = 0
        , pageSize = defaultPageSize
        , mapState = GMap.MapState (Encode.string "google map here") (GMap.LatLng 0 0)
        , showMap = False
        }
    , Cmd.batch
        [ getSearchTerms |> Http.toTask |> Task.attempt GetAllSearchTermsCompleted
        , initialParams |> List.map getSearchTerm |> List.map Http.toTask |> List.map (Task.attempt GetSearchTermCompleted) |> Cmd.batch
        , searchRequest [] 0 defaultPageSize 0 |> Http.toTask |> Task.attempt SearchCompleted
        , getProjectCounts |> Http.toTask |> Task.attempt GetProjectCountsCompleted
        ]
    )



-- UPDATE


type Msg
    = GetProjectCountsCompleted (Result Http.Error (List ProjectCount))
    | GetAllSearchTermsCompleted (Result Http.Error (List SearchTerm))
    | GetSearchTermCompleted (Result Http.Error SearchTerm)
    | ClearFilters
    | AddFilter PURL
    | RemoveFilter PURL
    | SetParamSearchInput String
    | ShowParamSearchDropdown
    | OpenStringFilterDialog SearchTerm
    | CloseStringFilterDialog
    | SetSearchFilterValue PURL String
    | SetStringFilterValue PURL String Bool
    | SetFilterValue PURL FilterValue
    | SetLocationFilterValue LocationFilterValue
    | SetProjectFilterValue String Bool
    | SetSortPos Int
    | SetPageSize Int
    | SetPageNum Int
    | InputTimerTick Time.Posix
    | Search Int
    | SearchCompleted (Result Http.Error SearchResponse)
    | MapTick
    | JSMap Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetProjectCountsCompleted (Ok counts) ->
            ( { model | projectCounts = counts }, Cmd.none )


        GetProjectCountsCompleted (Err error) -> --TODO
            let
                _ = Debug.log "GetProjectCountsCompleted" (toString error)
            in
            ( model, Cmd.none )

        GetAllSearchTermsCompleted (Ok terms) ->
            ( { model | allParams = terms }, Cmd.none )

        GetAllSearchTermsCompleted (Err error) -> --TODO
            let
                _ = Debug.log "GetAllSearchTermsCompleted" (toString error)
            in
            ( model, Cmd.none )

        GetSearchTermCompleted (Ok term) ->
            let
                selectedTerms =
                    Dict.insert term.id term model.selectedTerms

                val =
                    case term.type_ of
                        "number" ->
                            RangeValue (toString term.min) (toString term.max)

                        _ ->
                            NoValue

                selectedVals =
                    Dict.insert term.id val model.selectedVals
            in
            ( { model | doSearch = True, selectedTerms = selectedTerms, selectedVals = selectedVals }, Cmd.none )

        GetSearchTermCompleted (Err error) -> --TODO
            let
                _ = Debug.log "GetSearchTermCompleted" (toString error)
            in
            ( model, Cmd.none )

        ClearFilters ->
            let
                newVals =
                    Dict.map (\k v -> NoValue) model.selectedVals
            in
            ( { model | doSearch = True, locationVal = NoLocationValue, selectedVals = newVals }, Cmd.none )

        AddFilter id ->
            let
                params =
                    model.selectedParams |> Set.fromList |> Set.insert id |> Set.toList

                getTerm =
                    getSearchTerm id |> Http.toTask
            in
            ( { model | paramSearchInputVal = "", selectedParams = params }, Task.attempt GetSearchTermCompleted getTerm )

        RemoveFilter id ->
            let
                newParams =
                    model.selectedParams |> Set.fromList |> Set.remove id |> Set.toList

                newTerms =
                    Dict.remove id model.selectedTerms

                newVals =
                    Dict.remove id model.selectedVals
            in
            ( { model | doSearch = True, selectedParams = newParams, selectedTerms = newTerms, selectedVals = newVals }, Cmd.none )

        SetParamSearchInput val ->
            ( { model | paramSearchInputVal = val }, Cmd.none )

        ShowParamSearchDropdown ->
            ( { model | showParamSearchDropdown = not model.showParamSearchDropdown }, Cmd.none )

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
                _ = Debug.log "SetStringFilterValue" (toString (id, val, enable))

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
                _ = Debug.log "SetFilterValue" (toString (id, val))

                newVals =
                    Dict.insert id val model.selectedVals
            in
            ( { model | doSearch = True, selectedVals = newVals }, Cmd.none )

        SetLocationFilterValue val ->
            ( { model | doSearch = True, locationVal = val }, Cmd.none )

        SetProjectFilterValue val enable ->
            let
                vals =
                    model.projectVals
                        |> Set.fromList
                        |> (if enable then Set.insert val else Set.remove val)
                        |> Set.toList
            in
            ( { model | doSearch = True, projectVals = vals }, Cmd.none )

        SetSortPos pos ->
            let
                newPos =
                    if pos == model.sortPos then
                        pos * -1
                    else
                        pos
            in
            ( { model | doSearch = True, sortPos = newPos }, Cmd.none )

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
            let
                _ = Debug.log "Search" (toString newPageNum)
            in
            case generateQueryParams model.locationVal model.projectVals model.selectedVals of
                Ok queryParams ->
                    let
                        searchTask =
                            searchRequest queryParams model.sortPos model.pageSize (model.pageSize * newPageNum) |> Http.toTask
                    in
                    ( { model | doSearch = False, isSearching = True, pageNum = newPageNum }, Task.attempt SearchCompleted searchTask )

                Err error ->
                    let
                        _ = Debug.log "Error generating query params:" (toString error)
                    in
                    ( model, Cmd.none )

        SearchCompleted (Ok response) ->
            ( { model | count = response.count, results = Just response.results, isSearching = False }, Cmd.none )

        SearchCompleted (Err error) ->
            let
                _ = Debug.log "SearchCompleted" (toString error)
            in
            ( { model | errorMsg = Just (toString error), isSearching = False }, Cmd.none )

        JSMap gmap ->
            ( { model | mapState = GMap.MapState gmap model.mapState.center }, Cmd.none )

        MapTick ->
            ( { model | showMap = True }, GMap.loadMap model.mapState.center )


getSearchTerms : Http.Request (List SearchTerm)
getSearchTerms =
    let
        url =
            apiBaseUrl ++ "/searchTerms"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list decodeSearchTerm))
        |> HttpBuilder.toRequest


getSearchTerm : PURL -> Http.Request SearchTerm
getSearchTerm id =
    let
        url =
            apiBaseUrl ++ "/searchTerms/" ++ id
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decodeSearchTerm)
        |> HttpBuilder.toRequest


defined : String -> Bool
defined s =
    s /= ""


validParam : FilterValue -> Bool
validParam val =
   case val of
        RangeValue min max ->
            defined min && defined max -- TODO check for valid number

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


generateQueryParams : LocationFilterValue -> List String -> Dict String FilterValue -> Result String (List (String, String))
generateQueryParams locationVal projectVals params =
    let
        range min max =
            "[" ++ min ++ "," ++ max ++ "]"

        offset val ofs =
            val ++ "," ++ ofs

        formatParam _ val = --TODO use encoder instead
            case val of
                RangeValue min max ->
                    range min max

                OffsetValue value ofs ->
                    offset value ofs --FIXME

                SearchValue s ->
                    "~" ++ s

                SingleValue s ->
                    s

                MultipleValues vals ->
                    String.join "|" vals

                DateTimeValue dt ->
                    dt

                DateTimeRangeValue dt1 dt2 ->
                    "[" ++ dt1 ++ "," ++ dt2 ++ "]"

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

        formatProjectParam vals =
            String.join "|" vals

        sortFirst id a b =
            if Tuple.first a == id then
                LT
            else if Tuple.first b == id then
                GT
            else
                EQ
    in
    --FIXME refactor section below
    if locationVal == NoLocationValue && projectVals == [] && Dict.isEmpty params then
        Ok [(purlSampleID, "")]
    else if params |> Dict.toList |> List.map Tuple.second |> List.all validParam then
        let
            queryParams =
                params
                    |> Dict.map formatParam
                    |> Dict.toList
                    |> List.sortWith (sortFirst purlDateTime) --FIXME kinda kludgey, another way to order time/space params properly
                    |> List.sortWith (sortFirst purlDepth)

            locParam =
                if validLocationParam locationVal then
                    [ ("location", formatLocationParam locationVal) ]
                else
                    []

            projectParam =
                if projectVals /= [] then
                    [ ("project", formatProjectParam projectVals) ]
                else
                    []
        in
        List.concat [ locParam, projectParam, [(purlSampleID, "")], queryParams ] |> Ok
    else
        Err "Invalid query parameter"


searchRequest : List (String, String) -> Int -> Int -> Int -> Http.Request SearchResponse
searchRequest queryParams sortPos limit offset =
    let
        url =
            apiBaseUrl ++ "/search"

        queryParams2 =
            queryParams
                |> List.append [ ("sort", toString sortPos) ]
                |> List.append [ ("limit", toString limit) ]
                |> List.append [ ("offset", toString offset) ]
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
    , results : List SearchResult --List Sample
    }


type alias SearchResult =
    { schemaId : Int
    , sampleId : Int
    , projectId : Int
    , projectName : String
    , values : List SearchResultValue
    }


type SearchResultValue
    = NoResultValue
    | NumberResultValue Float
    | StringResultValue String


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


type alias SearchTerm =
    { type_ : String
    , id : String
    , label : String
    , unitLabel : String
    , aliases : List String
    , min : Float
    , max : Float
    , values : Dict String Int --FIXME change to List (String, Int)
--    , viewType : String -- "range", "offset", "exact"
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
    , count : Int
    }


decodeSearchResponse : Decoder SearchResponse
decodeSearchResponse =
    Decode.succeed SearchResponse
        |> required "count" Decode.int
        |> required "results" (Decode.list decodeSearchResult)--(Decode.list decodeSample)


decodeSearchResult : Decoder SearchResult
decodeSearchResult =
    Decode.succeed SearchResult
        |> required "schemaId" Decode.int
        |> required "sampleId" Decode.int
        |> required "projectId" Decode.int
        |> required "projectName" Decode.string
        |> required "values" (Decode.list decodeSearchResultValue)


decodeSearchResultValue : Decoder SearchResultValue
decodeSearchResultValue =
    Decode.oneOf
        [ Decode.map NumberResultValue Decode.float
        , Decode.map StringResultValue Decode.string
        ]


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


decodeSearchTerm : Decoder SearchTerm
decodeSearchTerm =
    Decode.succeed SearchTerm
        |> required "type" Decode.string
        |> required "id" Decode.string
        |> required "label" Decode.string
        |> optional "unitLabel" Decode.string ""
        |> optional "aliases" (Decode.list Decode.string) []
        |> optional "min" Decode.float 0
        |> optional "max" Decode.float 0
        |> optional "values" (Decode.dict Decode.int) Dict.empty
--        |> optional "viewType" Decode.string ""


decodeProjectCount : Decoder ProjectCount
decodeProjectCount =
    Decode.succeed ProjectCount
        |> required "name" Decode.string
        |> required "count" Decode.int



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ style "float" "left", style "width" "26%" ]
            [ viewSearchPanel model ]
        , div [ class "float-right", style "width" "74%", class "container-fluid" ]
--            [ viewSearchSummary model
--            , br [] []
            [ viewMap model.showMap
            , viewResults model
            ]
        , case model.stringFilterDialogTerm of
            Nothing ->
                text ""

            Just term ->
                viewStringFilterDialog term (Dict.get term.id model.selectedVals |> Maybe.withDefault NoValue)
        ]


viewSearchPanel : Model -> Html Msg
viewSearchPanel model =
    div []
        [ h4 [ style "margin" "3px", style "display" "inline" ]
            [ text "Filters" ]
        , span [ class "float-right small", style "padding-top" "5px" ]
            [ a [ class "alert-link", href "#", onClick ClearFilters ] [ text "Reset" ]
            , text " | "
            , a [ class "alert-link", href "#"] [ text "Advanced Search" ]
        ]
        , div [ style "border" "1px solid lightgray" ]
            [ ul [ class "nav nav-tabs" ]
                [ li [ class "nav-item" ]
                    [ a [ class "nav-link" ] [ text "Projects" ] ]
                , li [ class "nav-item" ]
                    [ a [ class "nav-link active", href "#", style "font-weight" "bold" ] [ text "Samples" ] ]
                , li [ class "nav-item" ]
                    [ a [ class "nav-link" ] [ text "Files" ] ]
                ]
            , div []
                [ viewLocationPanel model
                , viewProjectPanel model.projectCounts
                , viewAddedFiltersPanel model.selectedParams model.selectedTerms model.selectedVals
                , viewAddFilterPanel model.showParamSearchDropdown model.paramSearchInputVal model.allParams model.selectedParams
                ]
            ]
        ]


viewLocationPanel : Model -> Html Msg
viewLocationPanel model =
    let
        depthVal =
            Dict.get purlDepth model.selectedVals |> Maybe.withDefault NoValue

        datetimeVal =
            Dict.get purlDateTime model.selectedVals |> Maybe.withDefault NoValue
    in
    div []
        [ div [ class "card", style "font-size" "0.85em" ]
            [ div [ class "card-body" ]
                [ h6 [ style "color" "darkblue"]
                    [ text (String.fromChar (Char.fromCode 9660))
                    , text " Time/Space"
                    , small [] [ a [ class "alert-link", href "#", class "float-right", onClick MapTick ] [ text "Map View" ] ]
                    ]
                , Html.form [ style "padding-top" "0.5em" ]
                    [ div [ class "form-row" ]
                        [ div [ class "input-group input-group-sm" ]
                            ((div [ class "input-group-prepend" ] [ span [ class "input-group-text", style "width" "6em" ] [ text "Location"] ])
                                :: (viewLocationFilterInput model.locationVal)
                                ++ [ div [ class "input-group-append" ]
                                    [ button [ class "btn btn-outline-secondary dropdown-toggle dropdown-toggle-split", type_ "button", attribute "data-toggle" "dropdown" ] [ text "Format" ]
                                    , div [ class "dropdown-menu" ]
                                        [ a [ class "dropdown-item", href "#", onClick (SetLocationFilterValue (LatLngRadiusValue ("","") "")) ] [ text "Lat, Lng (deg), Radius (m)" ]
--                                        , a [ class "dropdown-item", href "#", onClick (SetLocationFilterValue (  ("","") ("",""))) ] [ text "Lat min/max, Lng min/max (deg)" ]
--                                        , a [ class "dropdown-item", href "#", onClick (SetLocationFilterValue (LatLngValue "" "")) ] [ text "Lat, Lng (deg)" ]
                                        , a [ class "dropdown-item disabled", href "#", disabled True, onClick (SetLocationFilterValue (LonghurstValue "")) ] [ text "Longhurst Province - coming soon" ]
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
                                ++ [ viewNumberFilterFormatOptions purlDepth ]
                            )
                        ]
                    , br [] []
                    , div [ class "form-row" ]
                        [ div [ class "input-group input-group-sm" ]
                            ((div [ class "input-group-prepend" ] [ span [ class "input-group-text", style "width" "6em" ] [ text "Date/Time"] ])
                                :: (viewDateTimeFilterInput purlDateTime datetimeVal)
                                ++ [ div [ class "input-group-append" ]
                                       [ button [ class "btn btn-outline-secondary", type_ "button" ] [ text (String.fromChar (Char.fromCode 128197)) ] ]
                                   , viewDateTimeFilterFormatOptions purlDateTime
                                   ]
                            )
                        ]
                    , br [] []
                    ]
                ]
            ]
        ]


viewAddFilterPanel : Bool -> String -> List SearchTerm -> List PURL -> Html Msg
viewAddFilterPanel showDropdown searchVal allTerms selectedIDs =
    let
        makeOption term =
            let
                dis =
                    List.member term.id selectedIDs
            in
            a [ classList [ ("dropdown-item", True), ("disabled", dis) ], href "#", onClick (AddFilter term.id) ] [ term.label |> String.Extra.toSentenceCase |> text ]

        removeRedundantTerms term =
            [ "latitude coordinate measurement datum"
            , "longitude coordinate measurement datum"
            , "zero-dimensional temporal region"
            , "depth of water"
            , "specimen collection time measurement datum start"
            ]
                |> List.member (String.toLower term.label)
                |> not

        filterOnSearch term =
            String.contains searchVal term.label

        options =
            allTerms
                |> List.filter removeRedundantTerms
                |> List.filter filterOnSearch
                |> List.sortWith (\a b -> compare a.label b.label )
                |> List.map makeOption

        show =
            searchVal /= "" || showDropdown
    in
    viewPanel "" "Add Filter" "" False
        [ div [ class "input-group input-group-sm", style "position" "relative" ]
            [ input [ type_ "text", class "form-control", placeholder "Search parameters", value searchVal, onInput SetParamSearchInput ] []
            , div [ class "input-group-append" ]
                [ button [ class "btn btn-outline-secondary dropdown-toggle", type_ "button", onClick ShowParamSearchDropdown ] []
                , div [ class "dropdown-menu", classList [("show", show)], style "position" "absolute", style "left" "0px", style "max-height" "30vh", style "overflow-y" "auto" ] options
                ]
            ]
        ]


viewAddedFiltersPanel : List PURL -> Dict PURL SearchTerm -> Dict PURL FilterValue -> Html Msg
viewAddedFiltersPanel params terms vals  =
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
                                    viewDateTimeFilterPanel term termVal

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


viewProjectPanel : List ProjectCount -> Html Msg --TODO merge with viewStringFilterPanel
viewProjectPanel projectCounts =
    let
        viewRow projectCount =
            div []
                [ div [ class "form-check form-check-inline" ]
                    [ input [ class "form-check-input", type_ "checkbox", onCheck (SetProjectFilterValue projectCount.name) ] []
                    , label [ class "form-check-label" ] [ text projectCount.name ]
                    ]
                , div [ class "badge badge-secondary float-right" ] [ projectCount.count |> toFloat |> format myLocale |> text ]
                ]

        sortByCount a b =
            case compare a.count b.count of
                LT -> GT
                EQ -> EQ
                GT -> LT

        truncatedOptions =
            projectCounts |> List.sortWith sortByCount |> List.take maxNumPanelOptions

        numOptions =
            List.length projectCounts
    in
    viewPanel "" "Project" "" False
        [ div [] (List.map viewRow truncatedOptions)
        , if numOptions > maxNumPanelOptions then
            button [ class "btn btn-sm btn-link float-right" ] [ toString (numOptions - maxNumPanelOptions) ++ " More ..." |> text ]
          else
            viewBlank
        ]


viewStringFilterPanel : SearchTerm -> FilterValue -> Html Msg
viewStringFilterPanel term val =
    let
        numOptions =
            Dict.size term.values

        sortByCount a b =
            case compare (Tuple.second a) (Tuple.second b) of
                LT -> GT
                EQ -> EQ
                GT -> LT

        truncatedOptions =
            Dict.toList term.values |> List.sortWith sortByCount |> List.take maxNumPanelOptions
    in
    viewTermPanel term
        [ div [ style "max-height" "5em", style "overflow-y" "auto" ] (viewStringFilterOptions term val truncatedOptions)
        , if numOptions > maxNumPanelOptions then
            button [ class "btn btn-sm btn-link float-right", onClick (OpenStringFilterDialog term) ] [ toString (numOptions - maxNumPanelOptions) ++ " More ..." |> text ]
          else
            viewBlank
        ]


viewStringFilterOptions : SearchTerm -> FilterValue -> List (String, Int) -> List (Html Msg)
viewStringFilterOptions term val options =
    let
        isChecked name =
            (case val of
                SingleValue s ->
                    List.singleton s

                MultipleValues l ->
                    l

                _ ->
                    []
            ) |> List.member name

        viewRow (name, count) =
            div []
                [ div [ class "form-check form-check-inline" ]
                    [ input [ class "form-check-input", type_ "checkbox", checked (isChecked name), onCheck (SetStringFilterValue term.id name) ] []
                    , label [ class "form-check-label" ] [ name |> String.Extra.toSentenceCase |> text]
                    ]
                , div [ class "badge badge-secondary float-right" ] [ count |> toFloat |> format myLocale |> text ]
                ]
    in
    List.map viewRow options


viewStringFilterDialog : SearchTerm -> FilterValue -> Html Msg
viewStringFilterDialog term val =
    let
        sortByName a b =
            case compare (Tuple.first a) (Tuple.first b) of
                LT -> GT
                EQ -> EQ
                GT -> LT

        options =
            Dict.toList term.values |> List.sortWith sortByName
    in
    viewDialog (String.Extra.toSentenceCase term.label)
        [ div [] (viewStringFilterOptions term val options) ]
        [ button [ type_ "button", class "btn btn-secondary", onClick CloseStringFilterDialog ] [ text "Close" ] ]
        CloseStringFilterDialog


-- Our own Boostrap modal since elm-dialog has not yet been ported to Elm 0.19
viewDialog : String -> List (Html Msg) -> List (Html Msg) -> Msg -> Html Msg
viewDialog title body footer closeMsg =
    div []
        [ div [ class "modal fade show", tabindex -1, style "display" "block", attribute "role" "dialog" ]
            [ div [ class "modal-dialog", attribute "role" "document" ]
                [ div [ class "modal-content" ]
                    [ div [ class "modal-header" ]
                        [ h5 [ class "modal-title" ] [ text title ]
                        , button [ type_ "button", class "close", onClick closeMsg ] [ span [] [ text (String.fromChar (Char.fromCode 215)) ] ]
                        ]
                    , div [ class "modal-body" ] body
                    , div [ class "modal-footer" ] footer
                    ]
                ]
            ]
        , div [ class "modal-backdrop fade show" ] []
        ]


viewNumberFilterPanel : SearchTerm -> FilterValue -> Html Msg
viewNumberFilterPanel term val =
    viewTermPanel term
        [ div [ class "input-group input-group-sm" ]
            (List.append (viewNumberFilterInput term.id val)
                [ viewNumberFilterFormatOptions term.id
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


viewNumberFilterFormatOptions : PURL -> Html Msg
viewNumberFilterFormatOptions id =
    let
        viewOption (label, filterVal) =
            a [ class "dropdown-item", href "#", onClick (SetFilterValue id filterVal) ]
                [ label |> String.Extra.toSentenceCase |> text ]

        options =
            [ ("exact", SingleValue ""), ("range", RangeValue "" ""), ("offset", OffsetValue "" "") ]
    in
    div [ class "input-group-append" ]
        [ button [ class "btn btn-outline-secondary dropdown-toggle dropdown-toggle-split", type_ "button", attribute "data-toggle" "dropdown" ] [ text "Format" ]
        , div [ class "dropdown-menu" ]
            (List.map viewOption options)
        ]


viewDateTimeFilterPanel : SearchTerm -> FilterValue -> Html Msg
viewDateTimeFilterPanel term val =
    viewTermPanel term
        [ div [ class "input-group input-group-sm" ]
            (List.append (viewDateTimeFilterInput term.id val)
                [ viewDateTimeFilterFormatOptions term.id
                ]
            )
        ]


viewDateTimeFilterInput : PURL -> FilterValue -> List (Html Msg)
viewDateTimeFilterInput id val =
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
            a [ class "dropdown-item", href "#", onClick (SetFilterValue id filterVal) ]
                [ label |> String.Extra.toSentenceCase |> text ]

        options =
--            [ a [ class "dropdown-item", href "#" ] [ text "Time YY-MM-DD HH:MM:SS" ]
--            , a [ class "dropdown-item", href "#" ] [ text "Day YY-MM-DD" ]
--            , a [ class "dropdown-item", href "#" ] [ text "Year YYYY" ]
--            , a [ class "dropdown-item", href "#" ] [ text "Range YY-MM-DD HH:MM:SS, YY-MM-DD HH:MM:SS" ]
--            ]
            [ ("Time YY-MM-DD HH:MM:SS", DateTimeValue ""), ("Range YY-MM-DD HH:MM:SS, YY-MM-DD HH:MM:SS", DateTimeRangeValue "" "") ]
    in
    div [ class "input-group-append" ]
        [ button [ class "btn btn-outline-secondary dropdown-toggle dropdown-toggle-split", type_ "button", attribute "data-toggle" "dropdown" ] [ text "Format" ]
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
                    span [ class "float-right", style "cursor" "pointer", onClick (RemoveFilter id) ] [ text (String.fromChar (Char.fromCode 10005)) ]
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
--                    param ++ " between [" ++ (toString min) ++ "," ++ (toString max) ++ "]"
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
        mkTh index label =
            let
                pos =
                    index + 1

                dirSymbol =
                    if model.sortPos > 0 then
                        String.fromChar (Char.fromCode 9650)
                    else
                        String.fromChar (Char.fromCode 9660)

                lbl =
                    String.Extra.toSentenceCase
                        (if pos == abs model.sortPos then
                            label ++ " " ++ dirSymbol
                        else
                            label
                        )
            in
            th [ style "cursor" "pointer", onClick (SetSortPos pos) ] [ text lbl ]

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
            , if depthVal /= NoValue && validParam depthVal then
                "Depth"
              else
                ""
            , if datetimeVal /= NoValue && validParam datetimeVal then
                "Date/Time"
              else
                ""
            ]
            |> List.filter (\s -> defined s)

        paramNames =
            List.concat
                [ [ "Project Name"
                  , "Sample ID"
                  --, "Sample Name"
                  ]
                , timeSpaceParamNames
                , (model.selectedParams
                        |> List.filterMap
                            (\param ->
                                case Dict.get param model.selectedTerms of
                                    Nothing ->
                                        Nothing

                                    Just term ->
                                        Just term.label
                            )

                    )
                , [ "Cart "]
                ]

        columns =
            List.indexedMap mkTh paramNames

        mkTd label =
            td [] [ text label ]

        formatVal val =
            case val of
                StringResultValue s ->
                    s

                NumberResultValue n ->
                    toString n

                NoResultValue ->
                    ""

        addToCartButton =
            button [ class "btn btn-sm btn-outline-dark", style "font-size" "0.5em" ] [ text "Add" ]

        mkRow result =
            tr []
                (List.concat
                    [
                    --[ mkTd (toString result.sampleId) ]
                    --, [ mkTd "" ] -- sample name
                      [ mkTd result.projectName ]
                    ,  List.map (formatVal >> mkTd) result.values
                    , [ td [] [ addToCartButton ] ]
                    ])

        count =
            model.results |> Maybe.withDefault [] |> List.length

        pageInfo =
            div [ class "float-right", style "font-size" "0.9em" ]
                [ text "Showing "
                , model.pageNum * model.pageSize + 1 |> Basics.max 1 |> toString |> text
                , text " - "
                , model.pageNum * model.pageSize + model.pageSize |> Basics.max 1 |> Basics.min model.count |> toString |> text
                , text " of "
                , model.count |> toFloat |> format myLocale |> text
                , text " sample"
                , (if model.count /= 1 then "s" else "") |> text
                ]

        lastPageNum =
            toFloat(model.count) / toFloat(model.pageSize) |> floor

        pageControls =
            let
                sizeOption size =
                    a [ class "dropdown-item", href "#", onClick (SetPageSize size) ] [ text (toString size) ]

                pageOption label num =
                    let
                        dis =
                            num < 0 -- previous
                                || num == model.pageNum -- current
                                || num > lastPageNum -- next
                    in
                    li [ classList [ ("page-item", True), ("disabled", dis) ] ]
                        [ a [ class "page-link", href "#", onClick (SetPageNum num) ] [ text label ] ]
            in
            div [ style "padding" "0.5em", style "border" "1px solid lightgray" ]
                [ text "Show "
                , div [ class "dropup", style "display" "inline" ]
                    [ button [ class "btn btn-secondary dropdown-toggle", type_ "button", attribute "data-toggle" "dropdown" ] [ text (toString model.pageSize) ]
                    , div [ class "dropdown-menu" ]
                        (List.map sizeOption [20, 40, 60, 80, 100])
                    ]
                , text " results"
                , nav [ class "float-right" ]
                    [ ul [ class "pagination" ]
                        --FIXME code below is a little kludgey
                        [ pageOption "First" 0
                        , pageOption "Previous" (model.pageNum - 1)
                        , pageOption (toString (model.pageNum + 1)) model.pageNum
                        , if model.pageNum + 1 > lastPageNum then text "" else pageOption (toString (model.pageNum + 2)) (model.pageNum + 1)
                        , if model.pageNum + 2 > lastPageNum then text "" else pageOption (toString (model.pageNum + 3)) (model.pageNum + 2)
                        , if model.pageNum + 3 > lastPageNum then text "" else pageOption "..." (model.pageNum + 3)
                        , pageOption "Next" (model.pageNum + 1)
                        , pageOption "Last" lastPageNum
                        ]
                    ]
                ]

        content =
            if model.isSearching then
                text "Searching ..."
            else if count == 0 then
                text "No Results"
            else if model.errorMsg /= Nothing then
                div []
                    [ p [] [ text "An error occurred:" ]
                    , p [] [ text (model.errorMsg |> Maybe.withDefault "") ]
                    ]
            else
                div []
                    [ div [ style "border" "1px solid lightgray" ]
                        [ div [ style "display" "inline" ]
                            [ button [ class "btn btn-sm btn-link alert-link float-right", style "margin" "4px" ] [ text "Columns" ]
                            , ul [ class "nav nav-tabs" ]
                                [ li [ class "nav-item" ]
                                    [ a [ class "nav-link" ] [ text "Summary" ] ]
                                , li [ class "nav-item" ]
                                    [ a [ class "nav-link" ] [ text "Projects" ] ]
                                , li [ class "nav-item" ]
                                    [ a [ class "nav-link active", href "#", style "font-weight" "bold" ] [ text "Samples" ] ]
                                , li [ class "nav-item" ]
                                    [ a [ class "nav-link" ] [ text "Files" ] ]
                                ]
                            ]
                        , table [ class "table table-sm table-striped", style "font-size" "0.85em" ]
                            [ thead [] [ tr [] columns ]
                            , tbody [] (model.results |> Maybe.withDefault [] |> List.map mkRow)
                            ]
                        ]
                    , pageControls
                    ]
    in
    div []
        [ h4 [ style "margin" "3px", style "display" "inline" ] [ text "Results" ]
        , if model.results /= Nothing then pageInfo else viewBlank
        , div [] [ content ]
        ]


viewMap : Bool -> Html Msg
viewMap showMap =
    let
        hideOrShow =
            if showMap then
                style "display" "block"
            else
                style "display" "none"
    in
    GMap.view [ hideOrShow, style "height" "90vh", style "width" "100%" ] []


viewBlank : Html Msg
viewBlank =
    text ""


viewSpinner : Html Msg
viewSpinner =
    div [ class "spinner-border" ] [ span [ class "sr-only" ] [ text "Loading..." ] ]
