import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
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
import Set
import Dict exposing (Dict)
import Debug exposing (toString)
import Config exposing (apiBaseUrl)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 500 InputTimerTick -- 500 ms
        ]



-- MODEL


type alias Model =
    { location : String
    , depth : String
    , date : String
    , allParams : List String
    , availableParams : List String
    , selectedParams : List String -- for maintaining order
    , selectedTerms : Dict String SearchTerm
    , selectedVals : Dict String (Maybe SearchTermValue)
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
        , availableParams = []
        , selectedParams = []
        , selectedTerms = Dict.empty
        , selectedVals = Dict.empty
        , doSearch = False
        , isSearching = False
        , searchStartTime = 0
        , results = Nothing
        , count = 0
        , errorMsg = Nothing
        , tableState = Table.initialSort "Sample"
        , pageNum = 0
        , pageSize = 50
        }
    , Cmd.batch
        [ getSearchTerms |> Http.toTask |> Task.attempt GetSearchTermsCompleted
        , searchRequest Dict.empty |> Http.toTask |> Task.attempt SearchCompleted
        ]
    )



-- UPDATE


type Msg
    = GetSearchTermsCompleted (Result Http.Error (List SearchTerm))
    | GetSearchTermCompleted (Result Http.Error SearchTerm)
--    | Search
    | SearchCompleted (Result Http.Error SearchResponse)
    | Clear
    | SetLatLngRadius String
    | SetDepth String
    | SetDate String
    | SelectParam String
    | SetStringParam String String
--    | Next
--    | Previous
    | SetTableState Table.State
    | InputTimerTick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetSearchTermsCompleted (Ok terms) ->
            let
                params =
                    List.map .label terms
            in
            ( { model | allParams = params }, Cmd.none )

        GetSearchTermsCompleted (Err error) -> --TODO
            ( model, Cmd.none )

        GetSearchTermCompleted (Ok term) ->
            let
--                terms =
--                    case term.type_ of
--                        "string" ->
--                            Just (StringTermValue term.values)
--
--                        "number" ->
--                            Just (NumberTermValue (term.min, term.max))
--
--                        _ ->
--                            Nothing

                selectedTerms =
                    Dict.insert term.label term model.selectedTerms

                val =
                    case term.type_ of
                        "string" ->
                            Just (StringTermValue "")

                        "number" ->
                            Just (NumberTermValue (term.min, term.max))

                        _ -> -- Error
                            Nothing

                selectedVals =
                    Dict.insert term.label val model.selectedVals
            in
            ( { model | selectedTerms = selectedTerms, selectedVals = selectedVals }, Cmd.none )

        GetSearchTermCompleted (Err error) -> --TODO
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
                    ("location" :: model.selectedParams) |> Set.fromList |> Set.toList

                term =
                    "[" ++ val ++ "]" |> StringTermValue |> Just

                vals =
                    Dict.insert "location" term model.selectedVals
            in
            ( { model | location = val, selectedParams = params, selectedVals = vals, doSearch = True }, Cmd.none )

        SetDepth val ->
            ( { model | depth = val, doSearch = True }, Cmd.none )

        SetDate val ->
            let
                params =
                    ("temporal" :: model.selectedParams) |> Set.fromList |> Set.toList

                term =
                    "[" ++ val ++ "]" |> StringTermValue |> Just

                vals =
                    Dict.insert "temporal" term model.selectedVals
            in
            ( { model | date = val, selectedParams = params, selectedVals = vals, doSearch = True }, Cmd.none )

        SelectParam name ->
            let
                params =
                    (name :: model.selectedParams) |> Set.fromList |> Set.toList

                getTerm =
                    getSearchTerm name |> Http.toTask
            in
            ( { model | selectedParams = params }, Task.attempt GetSearchTermCompleted getTerm )

        SetStringParam name val ->
            let
                _ = Debug.log ("SetStringParam " ++ name) val

                termVal =
                    case Dict.get name model.selectedTerms of
                        Nothing -> -- Error
                            Nothing

                        Just term ->
                            case term.type_ of
                                "string" ->
                                    Just (StringTermValue val)

                                _ -> -- Error
                                    Nothing

                vals =
                    Dict.insert name termVal model.selectedVals
            in
            ( { model | selectedVals = vals }, Cmd.none )

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
                    let
                        searchTask =
                            searchRequest model.selectedVals |> Http.toTask
                    in
                    ( { model | doSearch = False, isSearching = True }, Task.attempt SearchCompleted searchTask )
                else
                    ( model, Cmd.none )
            else
                ( model, Cmd.none )

        SearchCompleted (Ok response) ->
            ( { model | count = response.count, results = Just response.results, isSearching = False }, Cmd.none )

        SearchCompleted (Err error) ->
            let
                _ = Debug.log "Error" (toString error)
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


getSearchTerm : String -> Http.Request SearchTerm
getSearchTerm term =
    let
        url =
            apiBaseUrl ++ "/searchTerms/" ++ term
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decodeSearchTerm)
        |> HttpBuilder.toRequest


--search : String -> String -> String -> String -> String -> String -> String -> Int -> Int -> Http.Request SearchResponse
--search lat lng radius minDepth maxDepth startDate endDate limit skip =
--    let
--        url =
--            apiBaseUrl ++ "/search"
--
--        queryParams =
--            [ ("lat", lat)
--            , ("lng", lng)
--            , ("radius", radius)
--            , ("minDepth", minDepth)
--            , ("maxDepth", maxDepth)
--            , ("startDate", startDate)
--            , ("endDate", endDate)
--            , ("limit", toString limit)
--            , ("skip", toString skip)
--            ]
--    in
--    HttpBuilder.get url
--        |> HttpBuilder.withQueryParams queryParams
--        |> HttpBuilder.withExpect (Http.expectJson decodeSearchResponse)
--        |> HttpBuilder.toRequest


searchRequest : Dict String (Maybe SearchTermValue) -> Http.Request SearchResponse
searchRequest params =
    let
        url =
            apiBaseUrl ++ "/search"

        range min max = --TODO use encoder here instead
            "[" ++ (toString min) ++ "," ++ (toString max) ++ "]"

        format _ val =
            case val of
                Just (NumberTermValue (min, max)) ->
                    range min max

                Just (StringTermValue s) ->
                    s

                _ ->
                    ""

        queryParams =
            params |> Dict.map format |> Dict.toList
    in
    HttpBuilder.get url
        |> HttpBuilder.withQueryParams queryParams
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
    , values : List String
    }


type SearchTermValue
    = StringTermValue String
    | NumberTermValue (Float, Float) -- min/max


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
        |> optional "values" (Decode.list Decode.string) []



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ style "float" "left", style "width" "26%" ]
            [ viewSearchPanel model ]
        , div [ style "float" "right", style "width" "74%", class "container-fluid" ]
            [ viewSearchSummary model
            , br [] []
            , viewResults model
            ]
        ]


viewSearchPanel : Model -> Html Msg
viewSearchPanel model =
    div []
        [ ul [ class "nav nav-tabs" ]
            [ li [ class "nav-item" ]
                [ a [ class "nav-link" ] [ text "Projects" ] ]
            , li [ class "nav-item" ]
                [ a [ class "nav-link active", href "#" ] [ text "Samples" ] ]
            , li [ class "nav-item" ]
                [ a [ class "nav-link" ] [ text "Files" ] ]
            ]
        , viewLocationPanel model
        , viewProjectPanel model
        , viewBiomePanel model
        , viewAddAttributePanel model
        ]


viewLocationPanel : Model -> Html Msg
viewLocationPanel model =
    div []
        [ div [ class "card", style "font-size" "0.85em" ]
            [ div [ class "card-body" ]
                [ h6 [ style "color" "darkblue"] [ text (String.fromChar (Char.fromCode 9660)), text " Time/Space" ]
                , Html.form [ style "padding-top" "0.5em" ]
                    [ div [ class "form-row" ]
                        [ div [ class "input-group input-group-sm" ]
                            [ input [ type_ "text", class "form-control", size 5, placeholder "Location", value model.location, onInput SetLatLngRadius ] []
                            , div [ class "input-group-append" ]
                                [ button [ class "btn btn-outline-secondary", type_ "button" ] [ text "Map" ]
                                , button [ class "btn btn-outline-secondary dropdown-toggle dropdown-toggle-split", type_ "button", attribute "data-toggle" "dropdown" ] [ text "Format" ]
                                , div [ class "dropdown-menu" ]
                                    [ a [ class "dropdown-item", href "#" ] [ text "Lat, Lng (deg), Radius (m)" ]
                                    , a [ class "dropdown-item", href "#" ] [ text "Lat min, max, Lng min, max (deg)" ]
                                    , a [ class "dropdown-item", href "#" ] [ text "Lat, Lng (deg)" ]
                                    , a [ class "dropdown-item", href "#" ] [ text "Longhurst Province" ]
                                    ]
                                ]
                            ]
                        ]
                    , br [] []
                    , div [ class "form-row" ]
                        [ div [ class "input-group input-group-sm" ]
                            [ input [ type_ "text", class "form-control", size 5, placeholder "Depth", value model.depth, onInput SetDepth ] []
                            , div [ class "input-group-append" ]
                                [ button [ class "btn btn-outline-secondary dropdown-toggle", type_ "button", attribute "data-toggle" "dropdown" ] [ text "Format" ]
                                , div [ class "dropdown-menu" ]
                                    [ a [ class "dropdown-item", href "#" ] [ text "Exact depth (m)" ]
                                    , a [ class "dropdown-item", href "#" ] [ text "Depth, plus/minus (m)" ]
                                    , a [ class "dropdown-item", href "#" ] [ text "Depth min, max (m)" ]
                                    ]
                                ]
                            ]
                        ]
                    , br [] []
                    , div [ class "form-row" ]
                        [ div [ class "input-group input-group-sm" ]
                            [ input [ type_ "text", class "form-control", size 5, placeholder "Date/Time", value model.date, onInput SetDate ] []
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
        myLocale =
            { usLocale | decimals = 0 }

        mkRow name count =
            div []
                [ div [ class "form-check form-check-inline" ]
                    [ input [ class "form-check-input", type_ "checkbox" ] []
                    , label [ class "form-check-label" ] [ text name ]
                    ]
                , div [ class "badge badge-secondary", style "float" "right" ] [ count |> format myLocale |> text ]
                ]
    in
    div []
        [ div [ class "card", style "font-size" "0.85em" ]
            [ div [ class "card-body" ]
                [ h6 [ style "color" "darkblue"] [ text (String.fromChar (Char.fromCode 9660)), text " Projects" ]
                , mkRow "HOT - Hawaii Ocean Time Series" 1234
                , mkRow "OSD - Ocean Science Data" 56789
                , mkRow "TARA - Tara Oceans Expedition" 12345
                , button [ class "btn btn-sm btn-link", style "float" "right" ] [ text "23 More ..."]
                ]
            ]
        ]


viewBiomePanel : Model -> Html Msg
viewBiomePanel model =
    let
        myLocale =
            { usLocale | decimals = 0 }

        mkRow name count =
            div []
                [ div [ class "form-check form-check-inline" ]
                    [ input [ class "form-check-input", type_ "checkbox" ] []
                    , label [ class "form-check-label" ] [ text name ]
                    ]
                , div [ class "badge badge-secondary", style "float" "right" ] [ count |> format myLocale |> text ]
                ]
    in
    div []
        [ div [ class "card", style "font-size" "0.85em" ]
            [ div [ class "card-body" ]
                [ h6 [ style "color" "darkblue"] [ text (String.fromChar (Char.fromCode 9660)), text " Biomes" ]
                , mkRow "Marine biome (ENVO:447)" 1234
                , mkRow "Marine pelagic biome (ENVO:1000023)" 56789
                , mkRow "Marine reef biome (ENVO:1000029)" 12345
                , button [ class "btn btn-sm btn-link", style "float" "right" ] [ text "23 More ..."]
                ]
            ]
        ]


viewAddAttributePanel : Model -> Html Msg
viewAddAttributePanel model =
    let
        makeOption label =
            a [ class "dropdown-item", href "#" ] [ text label ]

        options =
            List.map makeOption model.allParams
    in
    div [ class "card", style "font-size" "0.85em" ]
        [ div [ class "card-body" ]
            [ h6 [ style "color" "darkblue"] [ text (String.fromChar (Char.fromCode 9660)), text " Add Filter" ]
            , div [ class "input-group input-group-sm" ]
                [ input [ type_ "text", class "form-control", size 5, placeholder "Search parameters", value "" ] []
                , div [ class "input-group-append" ]
                    [ button [ class "btn btn-outline-secondary dropdown-toggle", type_ "button", attribute "data-toggle" "dropdown" ] []
                    , div [ class "dropdown-menu" ] options
                    ]
                ]
            ]
        ]


--viewParams : Model -> Html Msg
--viewParams model =
--    let
--        mkOpt val =
--            option [] [ text val ]
--
--        mkRow termName =
--            case Dict.get termName model.selectedTerms of
--                Nothing ->
--                    div [] [ text "Loading..." ]
--
--                Just term ->
--                    case term.type_ of
--                        "string" ->
--                            div []
--                                [ text termName
--                                , select [ onInput (SetStringParam termName) ] (List.map mkOpt term.values)
--                                ]
--
--                        "number" ->
--                            div []
--                                [ text termName
--                                , input [ size 8, value (toString term.min) ] []
--                                , input [ size 8, value (toString term.max) ] []
--                                ]
--
--                        _ -> -- Error
--                            div [] []
--    in
--    div [] (List.map mkRow model.selectedParams)


viewSearchSummary : Model -> Html Msg
viewSearchSummary model =
    let
        format param =
            case Dict.get param model.selectedVals |> Maybe.withDefault Nothing of
                Nothing ->
                    "<error>"

                Just (StringTermValue s) ->
                    param ++ " = " ++ s

                Just (NumberTermValue (min, max)) ->
                    param ++ " between [" ++ (toString min) ++ "," ++ (toString max) ++ "]"

        searchStr =
            model.selectedParams |> List.map format |> String.join " AND "

        content =
            if model.selectedParams == [] then
                div []
                    [ text "Begin by selecting filters on the left or try the "
                    , a [ class "alert-link", href "#" ] [ text "Advanced Search" ]
                    ]
            else
                text searchStr
    in
    div [ class "card" ]
        [ div [ class "card-body" ]
            [ content ]
        ]


viewResults : Model -> Html Msg
viewResults model =
    let
        mkTh label =
            th [] [ text label ]

        columns =
            mkTh "Sample ID" :: (model.selectedParams |> List.map mkTh)

        mkTd label =
            td [] [ text label ]

        formatVal val =
            case val of
                StringResultValue s ->
                    s

                NumberResultValue n ->
                    toString n

        mkRow result =
            tr [] (mkTd (toString result.sampleId) :: (List.map (formatVal >> mkTd) result.values))

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
                                    table [ class "table table-sm table-striped", style "font-size" "0.85em" ]
                                        [ thead [] [ tr [] columns ]
                                        , tbody [] (List.map mkRow results)
                                        ]
    --                                    [ model.count |> toString |> text
    --                                    , text " total results. Showing "
    --                                    , model.pageSize |> toString |> text
    --                                    , text " results starting at result #"
    --                                    , (model.pageNum * model.pageSize) |> Basics.max 1 |> toString |> text
    --                                    , text ". "
    ----                                    , a [ onClick Previous ] [ text "Prev" ]
    ----                                    , text " / "
    ----                                    , a [ onClick Next ] [ text "Next" ]
    --                                    , br [] []
    --                                    , br [] []
    ----                                    , Table.view resultTableConfig model.tableState results
    --                                    , viewResultsTable results
    --                                    ]

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
