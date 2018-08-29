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


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


apiBaseUrl =
    "http://localhost:3010"


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
    , results : Maybe (List Sample)
    , errorMsg : Maybe String
    , tableState : Table.State
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
        , results = Nothing
        , errorMsg = Nothing
        , tableState = Table.initialSort "Sample"
        }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Search
    | SearchCompleted (Result Http.Error (List Sample))
    | SetLatitude String
    | SetLongitude String
    | SetRadius String
    | SetMinDepth String
    | SetMaxDepth String
    | SetStartDate String
    | SetEndDate String
    | SetTableState Table.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Search ->
            let
                doSearch =
                    search model.lat model.lng model.radius model.minDepth model.maxDepth model.startDate model.endDate |> Http.toTask
            in
            ( { model | errorMsg = Nothing, results = Nothing }, Task.attempt SearchCompleted doSearch)

        SearchCompleted (Ok results) ->
            ( { model | results = Just results }, Cmd.none )

        SearchCompleted (Err error) ->
            let
                _ = Debug.log "Error" (toString error)
            in
            ( { model | errorMsg = Just (toString error) }, Cmd.none )

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

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )


search : String -> String -> String -> String -> String -> String -> String -> Http.Request (List Sample)
search lat lng radius minDepth maxDepth startDate endDate =
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
            ]
    in
    HttpBuilder.get url
        |> HttpBuilder.withQueryParams queryParams
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list decodeSample))
        |> HttpBuilder.toRequest


type alias Sample =
    { sampleName : String
    , projectName : String
    , location : Location
    , depth : Int
    , date : String
    }


type alias Location =
    { type_ : String
    , coordinates : List Float
    }


decodeSample : Decoder Sample
decodeSample =
    Decode.succeed Sample
        |> required "sample" Decode.string
        |> required "project" Decode.string
        |> required "location" decodeLocation
        |> required "depth" Decode.int
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


viewResults : Model -> Html Msg
viewResults model =
    let
        content =
            case model.errorMsg of
                Nothing ->
                    case model.results of
                        Nothing ->
                            text "No results"

                        Just results ->
                            div []
                                [ List.length results |> toString |> text
                                , text " results"
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
            , Table.intColumn "Depth" .depth
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