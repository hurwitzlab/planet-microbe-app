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
    , isSearching : Bool
    , results : Maybe (List Sample)
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
        , isSearching = False
        , results = Nothing
        , count = 0
        , errorMsg = Nothing
        , tableState = Table.initialSort "Sample"
        , pageNum = 0
        , pageSize = 50
        }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Search
    | SearchCompleted (Result Http.Error Response)
    | SetExample String String String String String String String
    | SetLatitude String
    | SetLongitude String
    | SetRadius String
    | SetMinDepth String
    | SetMaxDepth String
    | SetStartDate String
    | SetEndDate String
    | Next
    | Previous
    | SetTableState Table.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Search ->
            let
                doSearch =
                    search model.lat model.lng model.radius model.minDepth model.maxDepth model.startDate model.endDate model.pageSize (model.pageNum * model.pageSize)
                        |> Http.toTask
            in
            ( { model | errorMsg = Nothing, results = Nothing, isSearching = True }, Task.attempt SearchCompleted doSearch)

        SearchCompleted (Ok response) ->
            ( { model | count = response.count, results = Just response.results, isSearching = False }, Cmd.none )

        SearchCompleted (Err error) ->
            let
                _ = Debug.log "Error" (toString error)
            in
            ( { model | errorMsg = Just (toString error), isSearching = False }, Cmd.none )

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


search : String -> String -> String -> String -> String -> String -> String -> Int -> Int -> Http.Request Response
search lat lng radius minDepth maxDepth startDate endDate limit skip =
    let
        url =
            apiBaseUrl ++ "/search"

        queryParams =
            [ ("lat", lat)
            , ("lng", lng)
            , ("radius", radius)
            , ("minDepth", minDepth)
            , ("maxDepth", maxDepth)
            , ("startDate", startDate)
            , ("endDate", endDate)
            , ("limit", toString limit)
            , ("skip", toString skip)
            ]
    in
    HttpBuilder.get url
        |> HttpBuilder.withQueryParams queryParams
        |> HttpBuilder.withExpect (Http.expectJson decodeResponse)
        |> HttpBuilder.toRequest


type alias Response =
    { count : Int
    , results : List Sample
    }


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


decodeResponse : Decoder Response
decodeResponse =
    Decode.succeed Response
        |> required "count" Decode.int
        |> required "results" (Decode.list decodeSample)


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



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "margin" "2em" ]
        [ h1 [] [ text "Planet Microbe Search Demo" ]
        , viewInputs model
        , viewExamples model
        , br [] []
        , br [] []
        , viewResults model
        ]


viewInputs : Model -> Html Msg
viewInputs model =
    div [ class "panel panel-default" ]
        [ div [ class "panel-body" ]
            [ Html.form [ class "form-inline" ]
                [ div [ class "form-group" ]
                    [ label [ attribute "for" "name" ] [ text "Latitude (deg):" ]
                    , text " "
                    , input [ type_ "text", class "form-control", size 5, placeholder "", value model.lat, onInput SetLatitude ] []
                    , text " "
                    , label [ attribute "for" "name" ] [ text "Longitude (deg):" ]
                    , text " "
                    , input [ type_ "text", class "form-control", size 5, placeholder "", value model.lng, onInput SetLongitude ] []
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
                                    , Table.view resultTableConfig model.tableState results
                                    ]

                    Just msg ->
                        div []
                            [ p [] [ text "An error occurred:" ]
                            , p [] [ text msg ]
                            ]
    in
    div [] [ content ]


resultTableConfig : Table.Config Sample Msg
resultTableConfig =
    Table.customConfig
        { toId = toString << .sampleName
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Project" .projectName
            , Table.stringColumn "Sample" .sampleName
            , latColumn
            , lngColumn
            , Table.floatColumn "Depth" .depth
            , Table.stringColumn "Date" .date
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = [ attribute "class" "table" ] }
        }


latColumn : Table.Column Sample Msg
latColumn =
    let
        lat sample =
            sample.location.coordinates |> List.Extra.getAt 1 |> Maybe.withDefault 0
    in
    Table.veryCustomColumn
        { name = "Latitude"
        , viewData =
            (\sample ->
                Table.HtmlDetails []
                    [ lat sample |> toString |> text
                    ]
            )
        , sorter = Table.increasingOrDecreasingBy lat
        }


lngColumn : Table.Column Sample Msg
lngColumn =
    let
        lng sample =
            sample.location.coordinates |> List.head |> Maybe.withDefault 0
    in
    Table.veryCustomColumn
        { name = "Longitude"
        , viewData =
            (\sample ->
                Table.HtmlDetails []
                    [ lng sample |> toString |> text
                    ]
            )
        , sorter = Table.increasingOrDecreasingBy lng
        }
