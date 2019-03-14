import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onCheck)
import Table exposing (defaultCustomizations)
import Http
import HttpBuilder
import Json.Encode as Encode exposing (Value, string)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Task
import Time
import List.Extra
import String.Extra
import Set
import Dict exposing (Dict)
import Debug exposing (toString)
import Config exposing (apiBaseUrl)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions model =
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
    50


facets =
    [ ("Biomes", "http://purl.obolibrary.org/obo/ENVO_00000428")
    ]



-- MODEL


type alias PURL =
    String


type alias Model =
    { location : String
    , depth : String
    , date : String
    , allParams : List SearchTerm -- list of available params to add
--    , availableParams : List String -- based on params already selected
    , selectedParams : List PURL -- added params, for maintaining order
    , selectedTerms : Dict PURL SearchTerm
    , selectedVals : Dict PURL FilterValue
    , locationVal : LocationFilterValue
    , doSearch : Bool
    , isSearching : Bool
    , searchStartTime : Int -- milliseconds
    , results : Maybe (List SearchResult) --(List Sample)
    , count : Int
    , errorMsg : Maybe String
    , tableState : Table.State
    , pageNum : Int
    , pageSize : Int
    }


-- lat/lon (constrained to -180/180, -90/90, respectively), date, depth.
init : Value -> ( Model, Cmd Msg )
init flags =
    (
        { location = ""
        , depth = ""
        , date = ""
        , allParams = []
--        , availableParams = []
        , selectedParams = []
        , selectedTerms = Dict.empty
        , selectedVals = Dict.empty
        , locationVal = LatLngRadiusValue ("", "") ""
        , doSearch = False
        , isSearching = False
        , searchStartTime = 0
        , results = Nothing
        , count = 0
        , errorMsg = Nothing
        , tableState = Table.initialSort "Sample"
        , pageNum = 0
        , pageSize = defaultPageSize
        }
    , Cmd.batch
        [ getSearchTerms |> Http.toTask |> Task.attempt GetAllSearchTermsCompleted
        , getSearchTerm "ENVO_00000428" |> Http.toTask |> Task.attempt GetSearchTermCompleted
        , searchRequest [] defaultPageSize |> Http.toTask |> Task.attempt SearchCompleted
        ]
    )



-- UPDATE


type Msg
    = GetAllSearchTermsCompleted (Result Http.Error (List SearchTerm))
    | GetSearchTermCompleted (Result Http.Error SearchTerm)
--    | Search
    | SearchCompleted (Result Http.Error SearchResponse)
    | Clear
    | SetLatLngRadius String
    | SetDepth String
    | SetDate String
    | AddFilter PURL
    | RemoveFilter PURL
    | SetSearchFilterValue PURL String
    | SetStringFilterValue PURL String Bool
    | SetFilterValue PURL FilterValue
    | SetLocationFilterValue LocationFilterValue
--    | Next
--    | Previous
    | SetTableState Table.State
    | InputTimerTick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

--        Search ->
--            let
--                doSearch =
--                    model.selectedVals |> search |> Http.toTask
--            in
--            ( { model | errorMsg = Nothing, results = Nothing, isSearching = True }, Task.attempt SearchCompleted doSearch)

        Clear ->
            ( { model | location = "", depth = "", date = "", selectedParams = [], selectedVals = Dict.empty, selectedTerms = Dict.empty, results = Nothing, errorMsg = Nothing }, Cmd.none )

        SetLatLngRadius val ->
            let
                params =
                    model.selectedParams |> Set.fromList |> Set.toList

                term =
                    SingleValue ("[" ++ val ++ "]")

                vals =
                    Dict.insert "location" term model.selectedVals
            in
            ( { model | location = val, selectedParams = params, selectedVals = vals, doSearch = True }, Cmd.none )

        SetDepth val ->
            ( { model | depth = val, doSearch = True }, Cmd.none )

        SetDate val ->
            let
                params =
                    model.selectedParams |> Set.fromList |> Set.toList

                term =
                    "[" ++ val ++ "]" |> SingleValue

                vals =
                    Dict.insert "temporal" term model.selectedVals
            in
            ( { model | date = val, selectedParams = params, selectedVals = vals, doSearch = True }, Cmd.none )

        AddFilter id ->
            let
                params =
                    model.selectedParams |> Set.fromList |> Set.insert id |> Set.toList

                getTerm =
                    getSearchTerm id |> Http.toTask
            in
            ( { model | selectedParams = params }, Task.attempt GetSearchTermCompleted getTerm )

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

                                SingleValue val1 -> --FIXME merge into MultipleValues case?258
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
            ( { model | doSearch = True, locationVal = val }, Cmd.none )

--        Next ->
--            let
--                (newModel, newCmd) =
--                    update Search { model | pageNum = model.pageNum + 1 }
--            in
--            ( newModel, newCmd )
--
--        Previous ->
--            let
--                pageNum =
--                    model.pageNum - 1 |> Basics.max 0
--
--                (newModel, newCmd) =
--                    update Search { model | pageNum = pageNum }
--            in
--            ( newModel, newCmd )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )

        InputTimerTick time ->
            if model.doSearch then
                if model.selectedVals == Dict.empty then
                    if model.results == Nothing then -- initial load of all samples
                        ( { model | doSearch = False }, Cmd.none ) --Task.attempt UpdateSamples (Request.Sample.list session.token |> Http.toTask) )
                    else
                        ( { model | doSearch = False, isSearching = False, results = Nothing }, Cmd.none )
                else if Time.posixToMillis time - model.searchStartTime >= 500 then -- 500 ms
                    case generateQueryParams model.locationVal model.selectedVals of
                        Ok queryParams ->
                            let
                                searchTask =
                                    searchRequest queryParams model.pageSize |> Http.toTask
                            in
                            ( { model | doSearch = False, isSearching = True }, Task.attempt SearchCompleted searchTask )

                        Err error ->
                            let
                                _ = Debug.log "InputTimerTick" (toString error)
                            in
                            ( model, Cmd.none )
                else
                    ( model, Cmd.none )
            else
                ( model, Cmd.none )

        SearchCompleted (Ok response) ->
            ( { model | count = response.count, results = Just response.results, isSearching = False }, Cmd.none )

        SearchCompleted (Err error) ->
            let
                _ = Debug.log "SearchCompleted" (toString error)
            in
            ( { model | errorMsg = Just (toString error), isSearching = False }, Cmd.none )


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


generateQueryParams : LocationFilterValue -> Dict String FilterValue -> Result String (List (String, String))
generateQueryParams locationVal params =
    let
        defined s =
            s /= ""

        validParam val =
           case val of
                RangeValue min max ->
                    defined min && defined max -- TODO check for valid number

                OffsetValue value ofs ->
                    defined value && defined ofs -- TODO check for valid number

                SearchValue s ->
                    defined s

                SingleValue s ->
                    defined s

                MultipleValues vals ->
                    List.all defined vals

--                LatLngValue lat lng ->
--                    defined lat && defined lng
--
--                LatLngRangeValue (lat1,lng1) (lat2,lng2) ->
--                    defined lat1 && defined lng1 && defined lat2 && defined lng2
--
--                LatLngRadiusValue (lat,lng) radius ->
--                    defined lat && defined lng && defined radius

                NoValue ->
                    True

        validLocationParam val =
            case val of
                LatLngValue lat lng ->
                    defined lat && defined lng

                LatLngRangeValue (lat1,lng1) (lat2,lng2) ->
                    defined lat1 && defined lng1 && defined lat2 && defined lng2

                LatLngRadiusValue (lat,lng) radius ->
                    defined lat && defined lng && defined radius

                LonghurstValue s ->
                    defined s

                NoLocationValue ->
                    True

        range min max = --TODO use encoder here instead
            "[" ++ min ++ "," ++ max ++ "]"

        offset val ofs = --TODO use encoder here instead
            "{" ++ val ++ "," ++ ofs ++ "}"

        formatParam _ val =
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
                    String.join "," vals

                NoValue ->
                    ""

        formatLocationParam val =
            case val of
                LatLngValue lat lng ->
                    range lat lng

                LatLngRangeValue (lat1,lng1) (lat2,lng2) ->
                    "[" ++ lat1 ++ "," ++ lng1 ++ "-" ++ lat2 ++ "," ++ lng2 ++ "]" --TODO use encoder here instead

                LatLngRadiusValue (lat,lng) radius ->
                    "[" ++ lat ++ "," ++ lng ++ "," ++ radius ++ "]" --TODO use encoder here instead

                LonghurstValue s ->
                    s

                NoLocationValue ->
                    ""
    in
    if Dict.isEmpty params then
        Ok []
    else if params |> Dict.toList |> List.map Tuple.second |> List.all validParam then
        let
            queryParams =
                params |> Dict.map formatParam |> Dict.toList

            allQueryParams =
                if validLocationParam locationVal then
                    ("location", formatLocationParam locationVal) :: queryParams
                else
                    queryParams
        in
        allQueryParams |> Ok
    else
        Err "Invalid query parameter"


searchRequest : List (String, String) -> Int -> Http.Request SearchResponse
searchRequest queryParams pageSize =
    let
        url =
            apiBaseUrl ++ "/search"

        queryParams2 =
            queryParams |> List.append [ ("limit", toString pageSize) ]
    in
    HttpBuilder.get url
        |> HttpBuilder.withQueryParams queryParams2
        |> HttpBuilder.withExpect (Http.expectJson decodeSearchResponse)
        |> HttpBuilder.toRequest


type alias SearchResponse =
    { count : Int
    , results : List SearchResult --List Sample
    }


type alias SearchResult =
    { schemaId : Int
    , sampleId : Int
    , values : List SearchResultValue
    }


type SearchResultValue
    = NumberResultValue Float
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
    , aliases : List String
    , min : Float
    , max : Float
    , values : Dict String Int --FIXME change to List (String, Int)
--    , viewType : String -- "range", "offset", "exact"
    }


type FilterValue
    = NoValue
    | SingleValue String
    | MultipleValues (List String)
    | SearchValue String
    | RangeValue String String -- numeric min/max
    | OffsetValue String String -- numeric +/-


type LocationFilterValue
    = NoLocationValue
    | LatLngValue String String -- latitude/longitude
    | LatLngRangeValue (String, String) (String, String) -- latitude/longitude to latitude/longitude
    | LatLngRadiusValue (String, String) String -- latitude/longitude with radius
    | LonghurstValue String -- Longhurst province


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
        |> optional "aliases" (Decode.list Decode.string) []
        |> optional "min" Decode.float 0
        |> optional "max" Decode.float 0
        |> optional "values" (Decode.dict Decode.int) Dict.empty
--        |> optional "viewType" Decode.string ""



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ style "float" "left", style "width" "26%" ]
            [ viewSearchPanel model ]
        , div [ style "float" "right", style "width" "74%", class "container-fluid" ]
--            [ viewSearchSummary model
--            , br [] []
            [ viewResults model
            ]
        ]


viewSearchPanel : Model -> Html Msg
viewSearchPanel model =
    let
        biomeTerm =
            Dict.get "http://purl.obolibrary.org/obo/ENVO_00000428" model.selectedTerms
    in
    div []
        [ h4 [ style "margin" "3px", style "display" "inline" ]
            [ text "Filters" ]
        , a [ class "alert-link", href "#", style "float" "right", style "font-size" "0.85em" ] [ text "Advanced Search" ]
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
                [ viewAddFilterPanel model
                , viewAddedFiltersPanel model
                , viewLocationPanel model
                , viewProjectPanel model
                , case biomeTerm of
                    Nothing ->
                        text ""

                    Just term ->
                        viewStringFilterPanel term
                ]
            ]
        ]


viewLocationPanel : Model -> Html Msg
viewLocationPanel model =
    div []
        [ div [ class "card", style "font-size" "0.85em" ]
            [ div [ class "card-body" ]
                [ h6 [ style "color" "darkblue"]
                    [ text (String.fromChar (Char.fromCode 9660))
                    , text " Time/Space"
                    , small [] [ a [ class "alert-link", href "#", style "float" "right" ] [ text "Map View" ] ]
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
                                        , a [ class "dropdown-item", href "#", onClick (SetLocationFilterValue (LatLngRangeValue ("","") ("",""))) ] [ text "Lat min/max, Lng min/max (deg)" ]
                                        , a [ class "dropdown-item", href "#", onClick (SetLocationFilterValue (LatLngValue "" "")) ] [ text "Lat, Lng (deg)" ]
                                        , a [ class "dropdown-item", href "#", onClick (SetLocationFilterValue (LonghurstValue "")) ] [ text "Longhurst Province" ]
                                        ]
                                    ]
                                ]
                            )
                        ]
                    , br [] []
                    , div [ class "form-row" ]
                        [ div [ class "input-group input-group-sm" ]
                            [ div [ class "input-group-prepend" ] [ span [ class "input-group-text", style "width" "6em" ] [ text "Depth"] ]
                            , input [ type_ "text", class "form-control", size 5, placeholder "", value model.depth, onInput SetDepth ] []
                            , div [ class "input-group-append" ]
                                [ button [ class "btn btn-outline-secondary dropdown-toggle", type_ "button", attribute "data-toggle" "dropdown" ] [ text "Format" ]
                                , div [ class "dropdown-menu" ]
                                    [ a [ class "dropdown-item", href "#" ] [ text "Exact depth (m)" ]
                                    , a [ class "dropdown-item", href "#" ] [ text "Depth, plus/minus (m)" ]
                                    , a [ class "dropdown-item", href "#" ] [ text "Depth min/max (m)" ]
                                    ]
                                ]
                            ]
                        ]
                    , br [] []
                    , div [ class "form-row" ]
                        [ div [ class "input-group input-group-sm" ]
                            [ div [ class "input-group-prepend" ] [ span [ class "input-group-text", style "width" "6em" ] [ text "Date/Time"] ]
                            , input [ type_ "text", class "form-control", size 5, placeholder "", value model.date, onInput SetDate ] []
                            , div [ class "input-group-append" ]
                                [ button [ class "btn btn-outline-secondary dropdown-toggle", type_ "button", attribute "data-toggle" "dropdown" ] [ text "Format" ]
                                , div [ class "dropdown-menu" ]
                                    [ a [ class "dropdown-item", href "#" ] [ text "Time YY-MM-DD HH:MM:SS" ]
                                    , a [ class "dropdown-item", href "#" ] [ text "Day YY-MM-DD" ]
                                    , a [ class "dropdown-item", href "#" ] [ text "Year YYYY" ]
                                    , a [ class "dropdown-item", href "#" ] [ text "Range YY-MM-DD HH:MM:SS, YY-MM-DD HH:MM:SS" ]
                                    ]
                                ]
                            ]
                        ]
                    , br [] []
                    ]
                ]
            ]
        ]


viewProjectPanel : Model -> Html Msg
viewProjectPanel model =
    let
        mkRow name count =
            div []
                [ div [ class "form-check form-check-inline" ]
                    [ input [ class "form-check-input", type_ "checkbox" ] []
                    , label [ class "form-check-label" ] [ text name ]
                    ]
                , div [ class "badge badge-secondary", style "float" "right" ] [ count |> format myLocale |> text ]
                ]
    in
    viewPanel "" "Project" False
        [ mkRow "HOT - Hawaii Ocean Time Series" 1234
        , mkRow "OSD - Ocean Science Data" 56789
        , mkRow "TARA - Tara Oceans Expedition" 12345
        , button [ class "btn btn-sm btn-link", style "float" "right" ] [ text "23 More ..."]
        ]


viewAddFilterPanel : Model -> Html Msg
viewAddFilterPanel model =
    let
        makeOption term =
            a [ class "dropdown-item", href "#", onClick (AddFilter term.id) ] [ term.label |> String.Extra.toSentenceCase |> text ]

        hideOptions =
            [ "biome", "latitude coordinate measurement datum", "longitude coordinate measurement datum", "zero-dimensional temporal region" ]

        options =
            model.allParams |> List.Extra.dropWhile (\term -> List.member term.label hideOptions) |> List.map makeOption -- FIXME why isn't this working?
    in
    viewPanel "" "Add Filter" False
        [ div [ class "input-group input-group-sm" ]
            [ input [ type_ "text", class "form-control", placeholder "Search parameters", value "" ] []
            , div [ class "input-group-append" ]
                [ button [ class "btn btn-outline-secondary dropdown-toggle", type_ "button", attribute "data-toggle" "dropdown" ] []
                , div [ class "dropdown-menu" ] options
                ]
            ]
        ]


viewAddedFiltersPanel : Model -> Html Msg
viewAddedFiltersPanel model =
    div []
        (model.selectedParams
            |> List.map
                (\param ->
                    case Dict.get param model.selectedTerms of
                        Nothing ->
                            text ""

                        Just term ->
                            let
                                termVal =
                                    Dict.get param model.selectedVals |> Maybe.withDefault NoValue
                            in
                            case term.type_ of
                                "string" ->
                                    if Dict.size term.values >= minNumPanelOptionsForSearchBar then
                                        viewSearchFilterPanel term termVal
                                    else
                                        viewStringFilterPanel term

                                "number" ->
                                    viewNumberFilterPanel term termVal

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


viewStringFilterPanel : SearchTerm -> Html Msg
viewStringFilterPanel term =
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

        viewRow (name, count) =
            div []
                [ div [ class "form-check form-check-inline" ]
                    [ input [ class "form-check-input", type_ "checkbox", onCheck (SetStringFilterValue term.id name) ] []
                    , label [ class "form-check-label" ] [ name |> String.Extra.toSentenceCase |> text]
                    ]
                , div [ class "badge badge-secondary", style "float" "right" ] [ count |> toFloat |> format myLocale |> text ]
                ]
    in
    viewTermPanel term
        [ div [] (List.map viewRow truncatedOptions)
        , if numOptions > maxNumPanelOptions then
            button [ class "btn btn-sm btn-link", style "float" "right" ] [ toString (numOptions - maxNumPanelOptions) ++ " More ..." |> text ]
          else
            text ""
        ]


viewNumberFilterPanel : SearchTerm -> FilterValue -> Html Msg
viewNumberFilterPanel term val =
    let
        viewOption (label, filterVal) =
            a [ class "dropdown-item", href "#", onClick (SetFilterValue term.id filterVal) ] [ label |> String.Extra.toSentenceCase |> text ]

        options =
            [ ("exact", SingleValue ""), ("range", RangeValue "" ""), ("offset", OffsetValue "" "") ]
    in
    viewTermPanel term
        [ div [ class "input-group input-group-sm" ]
            (List.append (viewFilterInput term.id val)
                [ div [ class "input-group-append" ]
                    [ button [ class "btn btn-outline-secondary dropdown-toggle dropdown-toggle-split", type_ "button", attribute "data-toggle" "dropdown" ] [ text "Format" ]
                    , div [ class "dropdown-menu" ]
                        (List.map viewOption options)
                    ]
                ]
            )
        ]


viewFilterInput : PURL -> FilterValue -> List (Html Msg)
viewFilterInput id val =
    case val of
        SingleValue s ->
            [ input [ type_ "text", class "form-control", placeholder "value", value s, onInput (\p -> SetFilterValue id (SingleValue p)) ] []
            ]

        RangeValue min max ->
            [ input [ type_ "text", class "form-control", placeholder "min", value min, onInput (\p -> SetFilterValue id (RangeValue p max)) ] []
            , input [ type_ "text", class "form-control", placeholder "max", value max, onInput (\p -> SetFilterValue id (RangeValue min p)) ] []
            ]

        OffsetValue n offset ->
            [ input [ type_ "text", class "form-control", placeholder "value", value n, onInput (\p -> SetFilterValue id (OffsetValue p offset)) ] []
            , input [ type_ "text", class "form-control", placeholder "+/-", value offset, onInput (\p -> SetFilterValue id (OffsetValue n p)) ] []
            ]

        _ ->
            []


viewLocationFilterInput : LocationFilterValue -> List (Html Msg)
viewLocationFilterInput val =
    case val of
        LatLngValue lat lng ->
            [ input [ type_ "text", class "form-control", placeholder "lat", value lat, onInput (\p -> SetLocationFilterValue (LatLngValue p lng)) ] []
            , input [ type_ "text", class "form-control", placeholder "lng", value lng, onInput (\p -> SetLocationFilterValue (LatLngValue lat p)) ] []
            ]

        LatLngRangeValue (lat1,lng1) (lat2,lng2) ->
            [ input [ type_ "text", class "form-control", placeholder "lat1", value lat1, onInput (\p -> SetLocationFilterValue (LatLngRangeValue (p,lng1) (lat2,lng2))) ] []
            , input [ type_ "text", class "form-control", placeholder "lng1", value lng1, onInput (\p -> SetLocationFilterValue (LatLngRangeValue (lat1,p) (lat2,lng2))) ] []
            , text " "
            , input [ type_ "text", class "form-control", placeholder "lat2", value lat2, onInput (\p -> SetLocationFilterValue (LatLngRangeValue (lat1,lng2) (p,lng2))) ] []
            , input [ type_ "text", class "form-control", placeholder "lng2", value lng2, onInput (\p -> SetLocationFilterValue (LatLngRangeValue (lat1,lng2) (lat2,p))) ] []
            ]

        LatLngRadiusValue (lat,lng) radius ->
            [ input [ type_ "text", class "form-control", placeholder "lat", value lat, onInput (\p -> SetLocationFilterValue (LatLngRadiusValue (p,lng) radius)) ] []
            , input [ type_ "text", class "form-control", placeholder "lng", value lng, onInput (\p -> SetLocationFilterValue (LatLngRadiusValue (lat,p) radius)) ] []
            , input [ type_ "text", class "form-control", placeholder "radius", value radius, onInput (\p -> SetLocationFilterValue (LatLngRadiusValue (lat,lng) p)) ] []
            ]

        LonghurstValue s ->
            [ input [ type_ "text", class "form-control", placeholder "Longhurst province", value s, onInput (SetLocationFilterValue << LonghurstValue) ] []
            ]

        NoLocationValue ->
            []


viewTermPanel : SearchTerm -> List (Html Msg) -> Html Msg
viewTermPanel term nodes =
    viewPanel term.id term.label True nodes


viewPanel : PURL -> String -> Bool -> List (Html Msg) -> Html Msg
viewPanel id title removable nodes =
    let
        header =
            h6 [ style "color" "darkblue"]
                [ text (String.fromChar (Char.fromCode 9660))
                , text " "
                , text (String.Extra.toTitleCase title)
                , if removable then
                    span [ class "float-right", style "cursor" "pointer", onClick (RemoveFilter id) ] [ text (String.fromChar (Char.fromCode 10005)) ]
                  else
                    text ""
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
        mkTh label =
            th [] [ text (String.Extra.toSentenceCase label) ]

        paramNames =
            model.selectedParams
                |> List.filterMap
                    (\param ->
                        case Dict.get param model.selectedTerms of
                            Nothing ->
                                Nothing

                            Just term ->
                                Just term.label
                    )

        columns =
            List.concat
                [ [ mkTh ("Sample ID " ++ (String.fromChar (Char.fromCode 9660))) ] --FIXME hardcoded sort arrow for demo
                , [ mkTh "Sample Name" ]
                , [ mkTh "Project Name" ]
                , paramNames |> List.map mkTh
                , [ mkTh ("Cart") ]
                ]

        mkTd label =
            td [] [ text label ]

        formatVal val =
            case val of
                StringResultValue s ->
                    s

                NumberResultValue n ->
                    toString n

        addToCartButton =
            button [ class "btn btn-sm btn-outline-dark", style "font-size" "0.5em" ] [ text "Add" ]

        mkRow result =
            tr []
                (List.concat
                    [ [ mkTd (toString result.sampleId) ]
                    , [ mkTd "" ]
                    , [ mkTd "" ]
                    , List.map (formatVal >> mkTd) result.values
                    , [ td [] [ addToCartButton ] ]
                    ])

        content =
            if model.isSearching then
                text "Searching ..."
            else
                case model.errorMsg of
                    Nothing ->
                        case model.results of
                            Nothing ->
                                text "No results"

                            Just results ->
                                if results == [] then
                                    text "No results"
                                else
                                    div []
                                        [ div []
                                            [ h4 [ style "margin" "3px", style "display" "inline" ]
                                                [ text "Results" ]
                                            , div [ style "float" "right", style "font-size" "0.9em" ]
                                                [ text "Showing "
                                                , model.pageNum * model.pageSize + 1 |> Basics.max 1 |> toString |> text
                                                , text " - "
                                                , model.pageNum * model.pageSize + model.pageSize |> Basics.max 1 |> toString |> text
                                                , text " of "
                                                , model.count |> toFloat |> format myLocale |> text
                                                , text " sample"
                                                , (if model.count /= 1 then "s" else "") |> text
                                                ]
                                            ]
                                        , div [ style "border" "1px solid lightgray" ]
                                            [ ul [ class "nav nav-tabs" ]
                                                [ li [ class "nav-item" ]
                                                    [ a [ class "nav-link" ] [ text "Summary" ] ]
                                                , li [ class "nav-item" ]
                                                    [ a [ class "nav-link" ] [ text "Projects" ] ]
                                                , li [ class "nav-item" ]
                                                    [ a [ class "nav-link active", href "#", style "font-weight" "bold" ] [ text "Samples" ] ]
                                                , li [ class "nav-item" ]
                                                    [ a [ class "nav-link" ] [ text "Files" ] ]
                                                ]
                                            , table [ class "table table-sm table-striped", style "font-size" "0.85em" ]
                                                [ thead [] [ tr [] columns ]
                                                , tbody [] (List.map mkRow results)
                                                ]
                                            ]
                                        , div [ style "padding" "0.5em", style "border" "1px solid lightgray" ]
                                            [ text "Show "
                                            , div [ class "dropup", style "display" "inline" ]
                                                [ button [ class "btn btn-secondary dropdown-toggle", type_ "button", attribute "data-toggle" "dropdown" ] [ text (toString model.pageSize) ]
                                                , div [ class "dropdown-menu" ]
                                                    [ a [ class "dropdown-item", href "#" ] [ text "20" ]
                                                    , a [ class "dropdown-item", href "#" ] [ text "40" ]
                                                    , a [ class "dropdown-item", href "#" ] [ text "60" ]
                                                    , a [ class "dropdown-item", href "#" ] [ text "80" ]
                                                    , a [ class "dropdown-item", href "#" ] [ text "100" ]
                                                    ]
                                                ]
                                            , text " results"
                                            , nav [ style "float" "right" ]
                                                [ ul [ class "pagination" ]
                                                    [ li [ class "page-item" ] [ a [ class "page-link", href "#" ] [ text "Previous" ] ]
                                                    , li [ class "page-item" ] [ a [ class "page-link", href "#" ] [ text "1" ] ]
                                                    , li [ class "page-item" ] [ a [ class "page-link", href "#" ] [ text "2" ] ]
                                                    , li [ class "page-item" ] [ a [ class "page-link", href "#" ] [ text "3" ] ]
                                                    , li [ class "page-item" ] [ a [ class "page-link", href "#" ] [ text "..." ] ]
                                                    , li [ class "page-item" ] [ a [ class "page-link", href "#" ] [ text "Last" ] ]
                                                    , li [ class "page-item" ] [ a [ class "page-link", href "#" ] [ text "Next" ] ]
                                                    ]
                                                ]
                                            ]
                                        ]


                    Just msg ->
                        div []
                            [ p [] [ text "An error occurred:" ]
                            , p [] [ text msg ]
                            ]
    in
    content

--
--viewResultsTable : List SearchResult -> Html Msg
--viewResultsTable results =
--    table [ style "border-spacing" "5px", style "border-collapse" "separate" ] (List.map viewResultRow results)
--
--
--viewResultRow : SearchResult -> Html Msg
--viewResultRow result =
--    let
--        mkCol val =
--            td [ style "border-left" "1px solid #000" ] [ text val ]
--
--        valToString val =
--            case val of
--                NumberResultValue num ->
--                    toString num
--
--                StringResultValue str ->
--                    str
--
--        cols =
--            mkCol (toString result.sampleId) :: (List.map (valToString >> mkCol) result.values)
--    in
--    tr [] cols


--resultTableConfig : Table.Config SearchResult Msg
--resultTableConfig =
--    Table.customConfig
--        { toId = toString << .sampleId
--        , toMsg = SetTableState
--        , columns =
--            [ Table.intColumn "Sample ID" .sampleId
--            ]
--        , customizations =
--            { defaultCustomizations | tableAttrs = [ attribute "class" "table" ] }
--        }
--
--
--latColumn : Table.Column Sample Msg
--latColumn =
--    let
--        lat sample =
--            sample.location.coordinates |> List.Extra.getAt 1 |> Maybe.withDefault 0
--    in
--    Table.veryCustomColumn
--        { name = "Latitude"
--        , viewData =
--            (\sample ->
--                Table.HtmlDetails []
--                    [ lat sample |> toString |> text
--                    ]
--            )
--        , sorter = Table.increasingOrDecreasingBy lat
--        }
--
--
--lngColumn : Table.Column Sample Msg
--lngColumn =
--    let
--        lng sample =
--            sample.location.coordinates |> List.head |> Maybe.withDefault 0
--    in
--    Table.veryCustomColumn
--        { name = "Longitude"
--        , viewData =
--            (\sample ->
--                Table.HtmlDetails []
--                    [ lng sample |> toString |> text
--                    ]
--            )
--        , sorter = Table.increasingOrDecreasingBy lng
--        }


viewSpinner : Html Msg
viewSpinner =
    div [ class "spinner-border" ] [ span [ class "sr-only" ] [ text "Loading..." ] ]
