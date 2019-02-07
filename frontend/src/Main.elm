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
import Task
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
        []



-- MODEL


type alias Model =
    { lat : String
    , lng : String
    , radius : String
    , minDepth : String
    , maxDepth : String
    , startDate : String
    , endDate : String
    , allParams : List String
    , availableParams : List String
    , selectedParams : List String -- for maintaining order
    , selectedTerms : Dict String SearchTerm
    , selectedVals : Dict String (Maybe SearchTermValue)
    , isSearching : Bool
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
        { lat = ""
        , lng = ""
        , radius = ""
        , minDepth = ""
        , maxDepth = ""
        , startDate = ""
        , endDate = ""
        , allParams = []
        , availableParams = []
        , selectedParams = []
        , selectedTerms = Dict.empty
        , selectedVals = Dict.empty
        , isSearching = False
        , results = Nothing
        , count = 0
        , errorMsg = Nothing
        , tableState = Table.initialSort "Sample"
        , pageNum = 0
        , pageSize = 50
        }
    , Http.toTask getSearchTerms |> Task.attempt GetSearchTermsCompleted
    )



-- UPDATE


type Msg
    = GetSearchTermsCompleted (Result Http.Error (List SearchTerm))
    | GetSearchTermCompleted (Result Http.Error SearchTerm)
    | Search
    | SearchCompleted (Result Http.Error SearchResponse)
    | Clear
    | SetExample String String String String String String String
    | SetLatitude String
    | SetLongitude String
    | SetRadius String
    | SetMinDepth String
    | SetMaxDepth String
    | SetStartDate String
    | SetEndDate String
    | SelectParam String
    | SetStringParam String String
    | Next
    | Previous
    | SetTableState Table.State


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

        Search ->
            let
                doSearch =
                    search model.selectedVals |> Http.toTask
            in
            ( { model | errorMsg = Nothing, results = Nothing, isSearching = True }, Task.attempt SearchCompleted doSearch)

        SearchCompleted (Ok response) ->
            ( { model | count = response.count, results = Just response.results, isSearching = False }, Cmd.none )

        SearchCompleted (Err error) ->
            let
                _ = Debug.log "Error" (toString error)
            in
            ( { model | errorMsg = Just (toString error), isSearching = False }, Cmd.none )

        Clear ->
            ( { model | lat = "", lng = "", radius = "", minDepth = "", maxDepth = "", startDate = "", endDate = "", selectedParams = [], selectedVals = Dict.empty, selectedTerms = Dict.empty, results = Nothing, errorMsg = Nothing }, Cmd.none )

        SetExample lat lng radius minDepth maxDepth startDate endDate ->
            let
                (newModel, newCmd) =
                    update Search { model | lat = lat, lng = lng, radius = radius, minDepth = minDepth, maxDepth = maxDepth, startDate = startDate, endDate = endDate }
            in
            ( newModel, newCmd )

        SetLatitude val ->
            ( { model | lat = val }, Cmd.none )

        SetLongitude val ->
            ( { model | lng = val }, Cmd.none )

        SetRadius val ->
            ( { model | radius = val }, Cmd.none )

        SetMinDepth val ->
            ( { model | minDepth = val }, Cmd.none )

        SetMaxDepth val ->
            ( { model | maxDepth = val }, Cmd.none )

        SetStartDate val ->
            ( { model | startDate = val }, Cmd.none )

        SetEndDate val ->
            ( { model | endDate = val }, Cmd.none )

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

        Next ->
            let
                (newModel, newCmd) =
                    update Search { model | pageNum = model.pageNum + 1 }
            in
            ( newModel, newCmd )

        Previous ->
            let
                pageNum =
                    model.pageNum - 1 |> Basics.max 0

                (newModel, newCmd) =
                    update Search { model | pageNum = pageNum }
            in
            ( newModel, newCmd )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )


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


search : Dict String (Maybe SearchTermValue) -> Http.Request SearchResponse
search params =
    let
        _ = Debug.log "" (toString params)

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
    div [ style "margin" "2em" ]
        [ viewInputs model
        , viewExamples model
        , br [] []
        , br [] []
        , viewResults model
        ]


viewInputs : Model -> Html Msg
viewInputs model =
    let
        options =
            option [ disabled True, selected True ] [ text "Please select a parameter" ] :: (List.map (\label -> option [ value label ] [ text label ] ) model.allParams)
    in
    div [ class "panel panel-default" ]
        [ div [ class "panel-body" ]
            [ Html.form [ class "form-inline" ]
                [ div [ class "form-group" ]
                    [ label [ attribute "for" "name" ] [ text "Latitude (deg):" ]
                    , text " "
                    , input [ type_ "text", class "form-control", size 8, placeholder "", value model.lat, onInput SetLatitude ] []
                    , text " "
                    , label [ attribute "for" "name" ] [ text "Longitude (deg):" ]
                    , text " "
                    , input [ type_ "text", class "form-control", size 8, placeholder "", value model.lng, onInput SetLongitude ] []
                    , text " "
                    , label [ attribute "for" "name" ] [ text "Radius (m):" ]
                    , text " "
                    , input [ type_ "text", class "form-control", size 5, placeholder "", value model.radius, onInput SetRadius ] []
                    ]
                ]
            , br [] []
            , Html.form [ class "form-inline" ]
                [ div [ class "form-group" ]
                    [ label [ attribute "for" "name" ] [ text "Depth (m):" ]
                    , text " "
                    , input [ type_ "text", class "form-control", size 5, placeholder "", value model.minDepth, onInput SetMinDepth ] []
                    , text " to "
                    , input [ type_ "text", class "form-control", size 5, placeholder "", value model.maxDepth, onInput SetMaxDepth ] []
                    ]
                ]
            , br [] []
            , Html.form [ class "form-inline" ]
                [ div [ class "form-group" ]
                    [ label [ attribute "for" "name" ] [ text "Date:" ]
                    , text " "
                    , input [ type_ "text", class "form-control", size 24, placeholder "", value model.startDate, onInput SetStartDate ] []
                    , text " to "
                    , input [ type_ "text", class "form-control", size 24, placeholder "", value model.endDate, onInput SetEndDate ] []
                    , text " Can use YY-MM-DD or YY-MM-DDTHH:MM:SS"
                    ]
                ]
            , br [] []
            , Html.form [ class "form-inline" ]
                [ div [ class "form-group" ]
                    [ label [ attribute "for" "name" ] [ text "Attributes:" ]
                    , text " "
                    , select [ class "form-control", onInput SelectParam ] options
                    ]
                ]
            , br [] []
            , viewParams model
            , br [] []
            , button [ class "btn btn-default", onClick Clear ] [ text "Clear" ]
            , text " "
            , button [ class "btn btn-primary", onClick Search ] [ text "Search" ]
            ]
        ]


viewExamples : Model -> Html Msg
viewExamples model =
    div []
        [ a [ onClick (SetExample "22.7" "-158" "1000" "" "" "" "") ] [ text "Example 1" ]
        , text ", "
        , a [ onClick (SetExample "" "" "" "0" "1000" "" "") ] [ text "Example 2" ]
        , text ", "
        , a [ onClick (SetExample "" "" "" "" "" "1988-01-01" "1989-01-01") ] [ text "Example 3" ]
        ]


viewParams : Model -> Html Msg
viewParams model =
    let
        mkOpt val =
            option [] [ text val ]

        mkRow termName =
            case Dict.get termName model.selectedTerms of
                Nothing ->
                    div [] [ text "Loading..." ]

                Just term ->
                    case term.type_ of
                        "string" ->
                            div []
                                [ text termName
                                , select [ onInput (SetStringParam termName) ] (List.map mkOpt term.values)
                                ]

                        "number" ->
                            div []
                                [ text termName
                                , input [ size 8, value (toString term.min) ] []
                                , input [ size 8, value (toString term.max) ] []
                                ]

                        _ -> -- Error
                            div [] []
    in
    div [] (List.map mkRow model.selectedParams)


viewResults : Model -> Html Msg
viewResults model =
    let
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
                                div []
                                    [ model.count |> toString |> text
                                    , text " total results. Showing "
                                    , model.pageSize |> toString |> text
                                    , text " results starting at result #"
                                    , (model.pageNum * model.pageSize) |> Basics.max 1 |> toString |> text
                                    , text ". "
                                    , a [ onClick Previous ] [ text "Prev" ]
                                    , text " / "
                                    , a [ onClick Next ] [ text "Next" ]
                                    , br [] []
                                    , br [] []
--                                    , Table.view resultTableConfig model.tableState results
                                    , viewResultsTable results
                                    ]

                    Just msg ->
                        div []
                            [ p [] [ text "An error occurred:" ]
                            , p [] [ text msg ]
                            ]
    in
    div [] [ content ]


viewResultsTable : List SearchResult -> Html Msg
viewResultsTable results =
    table [ style "border-spacing" "5px", style "border-collapse" "separate" ] (List.map viewResultRow results)


viewResultRow : SearchResult -> Html Msg
viewResultRow result =
    let
        mkCol val =
            td [ style "border-left" "1px solid #000" ] [ text val ]

        valToString val =
            case val of
                NumberResultValue num ->
                    toString num

                StringResultValue str ->
                    str

        cols =
            mkCol (toString result.sampleId) :: (List.map (valToString >> mkCol) result.values)
    in
    tr [] cols


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
