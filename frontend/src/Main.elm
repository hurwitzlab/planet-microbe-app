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


myLocale =
    { usLocale | decimals = 0 }


defaultPageSize =
    20


maxNumPanelOptions =
    4


facets =
    [ "http://purl.obolibrary.org/obo/ENVO_00000428" -- biome
    ]



-- MODEL


type alias PURL =
    String


type alias Model =
    { location : String
    , depth : String
    , date : String
    , allParams : List SearchTerm
--    , availableParams : List String -- based on params already selected
    , selectedParams : List PURL -- for maintaining order
    , selectedTerms : Dict PURL SearchTerm
    , selectedVals : Dict PURL (Maybe SearchTermValue)
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
        , searchRequest Dict.empty defaultPageSize |> Http.toTask |> Task.attempt SearchCompleted
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
    | SetStringParam PURL String
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
                            Just (NumberTermValue (term.min, term.max))

--                        "string" ->
--                            Just (StringTermValue "")

                        _ -> -- Error
                            Nothing

                selectedVals =
                    Dict.insert term.id val model.selectedVals
            in
            ( { model | selectedTerms = selectedTerms, selectedVals = selectedVals }, Cmd.none )

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
                    model.selectedParams |> Set.fromList |> Set.toList

                term =
                    "[" ++ val ++ "]" |> StringTermValue |> Just

                vals =
                    Dict.insert "temporal" term model.selectedVals
            in
            ( { model | date = val, selectedParams = params, selectedVals = vals, doSearch = True }, Cmd.none )

        AddFilter id ->
            let
                params =
                    (id :: model.selectedParams) |> Set.fromList |> Set.toList

                getTerm =
                    getSearchTerm id |> Http.toTask
            in
            ( { model | selectedParams = params }, Task.attempt GetSearchTermCompleted getTerm )

        SetStringParam id val ->
            let
                _ = Debug.log ("SetStringParam " ++ id) val

                termVal =
                    case Dict.get id model.selectedTerms of
                        Nothing -> -- Error
                            Nothing

                        Just term ->
                            case term.type_ of
                                "string" ->
                                    Just (StringTermValue val)

                                _ -> -- Error
                                    Nothing

                vals =
                    Dict.insert id termVal model.selectedVals
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
                            searchRequest model.selectedVals model.pageSize |> Http.toTask
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


searchRequest : Dict String (Maybe SearchTermValue) -> Int -> Http.Request SearchResponse
searchRequest params pageSize =
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
            params |> Dict.map format |> Dict.toList |> List.append [ ("limit", toString pageSize) ]
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
--            [ viewSearchSummary model
--            , br [] []
            [ viewResults model
            ]
        ]


viewSearchPanel : Model -> Html Msg
viewSearchPanel model =
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
                , viewBiomePanel model
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
                            [ input [ type_ "text", class "form-control", size 5, placeholder "Location", value model.location, onInput SetLatLngRadius ] []
                            , div [ class "input-group-append" ]
                                [ button [ class "btn btn-outline-secondary dropdown-toggle dropdown-toggle-split", type_ "button", attribute "data-toggle" "dropdown" ] [ text "Format" ]
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
    viewStringFilterPanel "Biomes"
        [ "Marine biome (ENVO:447)", "Marine pelagic biome (ENVO:1000023)", "Marine reef biome (ENVO:1000029)" ]
        --[ ("Marine biome (ENVO:447)", 1234), ("Marine pelagic biome (ENVO:1000023)", 56789), ("Marine reef biome (ENVO:1000029)", 12345) ]


viewAddFilterPanel : Model -> Html Msg
viewAddFilterPanel model =
    let
        makeOption term =
            a [ class "dropdown-item", href "#", onClick (AddFilter term.id) ] [ text term.label ]

        hideOptions =
            [ "biome", "latitude coordinate measurement datum", "longitude coordinate measurement datum", "zero-dimensional temporal region" ]

        options =
            model.allParams |> List.Extra.dropWhile (\term -> List.member term.label hideOptions) |> List.map makeOption -- FIXME why isn't this working?
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


viewAddedFiltersPanel : Model -> Html Msg
viewAddedFiltersPanel model =
    let
        errorDiv =
            div [] [ text "<error>" ]
    in
    div []
        (model.selectedParams
            |> List.map
                (\param ->
                    case Dict.get param model.selectedTerms of
                        Nothing ->
                            errorDiv

                        Just term ->
                            case term.type_ of
                                "string" ->
                                    viewStringFilterPanel term.label term.values

                                "number" ->
                                    viewNumberFilterPanel term.label

                                _ ->
                                    errorDiv

                )
        )


viewStringFilterPanel : String -> List String -> Html Msg
viewStringFilterPanel title options =
    let
        numOptions =
            List.length options

        truncatedOptions =
            List.take maxNumPanelOptions options

        viewRow name =
            div []
                [ div [ class "form-check form-check-inline" ]
                    [ input [ class "form-check-input", type_ "checkbox" ] []
                    , label [ class "form-check-label" ] [ text name ]
                    ]
                , div [ class "badge badge-secondary", style "float" "right" ] [ text "123" ] --[ count |> toFloat |> format myLocale |> text ]
                ]
    in
    div []
        [ div [ class "card", style "font-size" "0.85em" ]
            [ div [ class "card-body" ]
                [ h6 [ style "color" "darkblue"] [ text (String.fromChar (Char.fromCode 9660)), text " ", text title ]
                , div [] (List.map viewRow truncatedOptions)
                , if numOptions > maxNumPanelOptions then
                    button [ class "btn btn-sm btn-link", style "float" "right" ] [ toString (numOptions - maxNumPanelOptions) ++ " More ..." |> text ]
                  else
                    text ""
                ]
            ]
        ]


viewNumberFilterPanel : String -> Html Msg
viewNumberFilterPanel title =
    div [] [ text "number" ]


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
            List.concat
                [ [ mkTh ("Sample ID " ++ (String.fromChar (Char.fromCode 9660))) ] --FIXME hardcoded sort arrow for demo
                , [ mkTh "Sample Name" ]
                , [ mkTh "Project Name" ]
                , model.selectedParams |> List.map mkTh
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
                                                , text " samples"
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
