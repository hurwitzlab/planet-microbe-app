module Search exposing (..)

import Http
import HttpBuilder
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as Encode exposing (null)
import Dict exposing (Dict)
import Html exposing (Html, a, text)
import Html.Attributes exposing (href, target)
import Config exposing (apiBaseUrl)



-- TYPES --


type alias Filter =
    { term : SearchTerm
    , value : FilterValue
    }


type alias SearchTerm =
    { type_ : String
    , id : PURL
    , label : String
    , definition : String
    , unitId : PURL
    , unitLabel : String
    , sourceUrl : String
    , alias_ : String
    , aliases : List Alias
    , annotations : List Annotation
    , distribution : Distribution
    -- Only for type "number"
    , min : Float
    , max : Float
    -- Only for type "string"
    , purlLabels : Dict String String
    }


defaultSearchTerm : SearchTerm
defaultSearchTerm =
    { type_ = "string"
    , id = ""
    , label = ""
    , definition = ""
    , unitId = ""
    , unitLabel = ""
    , sourceUrl = ""
    , alias_ = ""
    , aliases = []
    , annotations = []
    , distribution = []
    -- Only for type "number"
    , min = 0
    , max = 0
    -- Only for type "string"
    , purlLabels = Dict.empty
    }


type alias PURL =
    String


type alias Alias =
    { name : String
    , sourceName : String
    , sourceUrl : String
    }


type alias Annotation =
    { id : String
    , label : String
    , value : String
    }


type alias Distribution =
    List (String, Int)


type FilterValue
    = NoValue
    | SingleValue String -- numeric/string value
    | RangeValue String String -- numeric min/max
    | OffsetValue String String -- numeric +/-
    | SearchValue String -- string value for partial match
    | MultipleValues (List String) -- multiple string literal values
    | DateTimeValue String -- single datetime value
    | DateTimeRangeValue String String -- start/end datetime values
    | LatLngRadiusValue (String, String) String -- latitude/longitude with radius
    | LonghurstValue String -- Longhurst province
    | OntologyValue String (List (String, String)) -- search val and results (id, label)


type alias SearchResponse =
    { sampleCount : Int
    , fileCount : Int
    , sampleResults : List SampleResult
    , fileResults : List FileResult
    , fileIDs : List Int -- list of file IDs for all search results (for "Add All to Cart" button)
    , summary : List Distribution
    , map : Encode.Value --List MapResult
    , error : Maybe String
    }


type alias SampleResult =
    { schemaId : Int
    , sampleId : Int
    , sampleAccn : String
    , projectId : Int
    , projectName : String
    , files : List Int -- list of file IDs for this sample
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
    , sampleId : Int
    , sampleAccn : String
    , projectId : Int
    , projectName : String
    , source : String
    , strategy : String
    , selection : String
    , layout : String
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


type Value
    = StringValue String
    | IntValue Int
    | FloatValue Float



-- SERIALIZATION --


searchTermDecoder : Decoder SearchTerm
searchTermDecoder =
    Decode.succeed SearchTerm
        |> required "type" Decode.string
        |> required "id" Decode.string
        |> required "label" Decode.string
        |> optional "definition" Decode.string ""
        |> optional "unitId" Decode.string ""
        |> optional "unitLabel" Decode.string ""
        |> optional "sourceUrl" Decode.string ""
        |> optional "alias" Decode.string ""
        |> optional "aliases" (Decode.list aliasDecoder) []
        |> optional "annotations" (Decode.list annotationDecoder) []
        |> optional "distribution" distributionDecoder []
        |> optional "min" Decode.float 0
        |> optional "max" Decode.float 0
        |> optional "purlLabels" (Decode.dict Decode.string) Dict.empty


aliasDecoder : Decoder Alias
aliasDecoder =
    Decode.succeed Alias
        |> required "name" Decode.string
        |> required "sourceName" Decode.string
        |> required "sourceUrl" Decode.string


annotationDecoder : Decoder Annotation
annotationDecoder =
    Decode.succeed Annotation
        |> required "id" Decode.string
        |> required "label" Decode.string
        |> optional "value" Decode.string ""


distributionDecoder : Decoder (List (String, Int))
distributionDecoder =
    Decode.list (Decode.map2 Tuple.pair (Decode.index 0 Decode.string) (Decode.index 1 Decode.int))


decodeSearchResponse : Decoder SearchResponse
decodeSearchResponse =
    Decode.succeed SearchResponse
        |> required "sampleCount" Decode.int
        |> required "fileCount" Decode.int
        |> required "sampleResults" (Decode.list decodeSampleResult)
        |> required "fileResults" (Decode.list decodeFileResult)
        |> required "files" (Decode.list Decode.int)
        |> required "summary" (Decode.list distributionDecoder)
        |> optional "map" Decode.value null --(Decode.list decodeMapResult) []
        |> optional "error" (Decode.nullable Decode.string) Nothing


decodeSampleResult : Decoder SampleResult
decodeSampleResult =
    Decode.succeed SampleResult
        |> required "schemaId" Decode.int
        |> required "sampleId" Decode.int
        |> required "sampleAccn" Decode.string
        |> required "projectId" Decode.int
        |> required "projectName" Decode.string
        |> optional "files" (Decode.list Decode.int) []
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
        |> required "sampleId" Decode.int
        |> required "sampleAccn" Decode.string
        |> required "projectId" Decode.int
        |> required "projectName" Decode.string
        |> required "source" Decode.string
        |> required "strategy" Decode.string
        |> required "selection" Decode.string
        |> required "layout" Decode.string


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


valueDecoder : Decoder (Maybe Value)
valueDecoder =
    Decode.nullable
        (Decode.oneOf
            [ Decode.map StringValue Decode.string
            , Decode.map IntValue Decode.int
            , Decode.map FloatValue Decode.float
            ]
        )



-- REQUESTS --


searchRequest : List (String, String) -> Http.Request SearchResponse
searchRequest queryParams =
    let
        url =
            apiBaseUrl ++ "/search"

        body =
            Encode.object
                (List.map (Tuple.mapSecond Encode.string) queryParams)
    in
    HttpBuilder.post url
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson decodeSearchResponse)
        |> HttpBuilder.toRequest


searchDownloadRequest : List (String, String) -> Http.Request String
searchDownloadRequest queryParams =
    let
        url =
            apiBaseUrl ++ "/search"

        downloadParam =
            ( "download", Encode.bool True )

        body =
            Encode.object
                (downloadParam :: (List.map (Tuple.mapSecond Encode.string) queryParams))
    in
    HttpBuilder.post url
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect Http.expectString
        |> HttpBuilder.toRequest


fetchAllSearchTerms : Http.Request (List SearchTerm)
fetchAllSearchTerms =
    let
        url =
            apiBaseUrl ++ "/searchTerms"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list searchTermDecoder))
        |> HttpBuilder.toRequest


fetchSearchTerms : List PURL -> Http.Request (List SearchTerm)
fetchSearchTerms ids =
    let
        url =
            apiBaseUrl ++ "/searchTerms/" ++ (String.join "," ids)
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list searchTermDecoder))
        |> HttpBuilder.toRequest


searchOntologyTerms : String -> String -> Http.Request (List Annotation)
searchOntologyTerms name keyword =
    let
        url =
            apiBaseUrl ++ "/ontology/" ++ name ++ "/search/" ++ keyword
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list annotationDecoder))
        |> HttpBuilder.toRequest


fetchOntologySubclasses : String -> String -> Http.Request (List Annotation)
fetchOntologySubclasses name id =
    let
        url =
            apiBaseUrl ++ "/ontology/" ++ name ++ "/subclasses/" ++ id
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list annotationDecoder))
        |> HttpBuilder.toRequest



-- VIEWS --

viewValue : String -> Html msg
viewValue val =
    if String.startsWith "http://" val || String.startsWith "https://" val || String.startsWith "ftp://" val then
        a [ href val, target "_blank" ] [ text val ]
    else
        text val



-- HELPERS --


defined : String -> Bool
defined s =
    s /= ""


defaultFilter : PURL -> String -> Filter
defaultFilter id label =
    Filter { defaultSearchTerm | id = id, label = label } NoValue


validFilterValue : FilterValue -> Bool
validFilterValue val =
   case val of
        RangeValue min max ->
            validNumber min || validNumber max -- Either/both can defined

        OffsetValue value ofs ->
            validNumber value && validNumber ofs

        SearchValue s ->
            defined s

        SingleValue s ->
            defined s

        MultipleValues vals ->
            List.all defined vals

        DateTimeValue dt ->
            defined dt --TODO check for valid date format

        DateTimeRangeValue dt1 dt2 ->
            defined dt1 || defined dt2 --TODO check for valid date format

        LatLngRadiusValue (lat,lng) _ -> -- radius is optional
            validNumber lat && validNumber lng

        LonghurstValue s ->
            defined s

        OntologyValue s vals ->
            vals /= []

        NoValue ->
            True


validNumber : String -> Bool
validNumber s =
    String.toFloat s /= Nothing


filterValueToString : FilterValue -> String
filterValueToString val =
    let
        range items =
            "[" ++ (String.join "," items) ++ "]"

        offset val2 ofs =
            val2 ++ "," ++ ofs
    in
    case val of
        RangeValue min max ->
            range [ min, max ]

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
            range [ dt1, dt2 ]

        LatLngRadiusValue (lat,lng) radius ->
            let
                r =
                    if radius == "" then
                        "0"
                    else
                        radius
            in
            range [ lat, lng, r ]

        LonghurstValue s ->
            s

        OntologyValue s values ->
            values |> List.map Tuple.second |> String.join "|"

        NoValue ->
            ""


getFilterValue : PURL -> List Filter -> FilterValue
getFilterValue purl filters =
    filters
        |> List.filter (\f -> f.term.id == purl)
        |> List.head
        |> Maybe.map .value
        |> Maybe.withDefault NoValue


isStringFilterSelected : String -> FilterValue -> Bool
isStringFilterSelected name val =
    (case val of
        SingleValue s ->
            [ s ]

        MultipleValues l ->
            l

        _ ->
            []
    )
    |> List.member name


updateFilterValue : PURL -> FilterValue -> List Filter -> List Filter
updateFilterValue purl newValue filters =
    filters
        |> List.map
            (\f ->
                if f.term.id == purl then
                    { f | value = newValue }
                else
                    f
            )


resetFilter : Filter -> Filter
resetFilter filter =
    { filter | value = NoValue }


updateFilter : PURL -> Filter -> List Filter -> List Filter
updateFilter purl newFilter filters =
    let
        exists =
            filters
                |> List.map (.term >> .id)
                |> List.member purl
    in
    if exists then
        filters
            |> List.map
                (\f ->
                    if f.term.id == purl then
                        newFilter
                    else
                        f
                )
    else
        newFilter :: filters


searchResultValuesToString : SearchResultValues -> String
searchResultValuesToString vals =
    (case vals of
        NoResultValues ->
            []

        SingleResultValue val ->
            [ val ]

        MultipleResultValues l ->
            l
    )
    |> List.map searchResultValueToString
    |> String.join ", "


searchResultValueToString : SearchResultValue -> String
searchResultValueToString val =
    case val of
        StringResultValue s ->
            s

        NumberResultValue n ->
            String.fromFloat n

        NoResultValue ->
            ""
