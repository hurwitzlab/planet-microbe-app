module Page.SamplingEvent exposing (Model, Msg, init, toSession, subscriptions, update, view)

import Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page
import Route
import SamplingEvent exposing (SamplingEvent, Data)
import Sample exposing (Sample)
import SearchTerm
import LatLng
import GMap
import Time
import Page
import Http
import RemoteData exposing (RemoteData(..))
import Task exposing (Task)
import String.Extra
import Json.Encode as Encode
--import Debug exposing (toString)



---- MODEL ----


type alias Model =
    { session : Session
    , samplingEvent : Maybe SamplingEvent
    , samples : Maybe (List Sample)
    , niskinData : RemoteData Http.Error Data
    , ctdData : RemoteData Http.Error Data
    , mapLoaded : Bool
    }


init : Session -> Int -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , samplingEvent = Nothing
      , samples = Nothing
      , niskinData = Loading
      , ctdData = Loading
      , mapLoaded = False
      }
      , Cmd.batch
        [ GMap.removeMap "" -- workaround for blank map on navigating back to this page
        , GMap.changeMapSettings (GMap.Settings False False True False |> GMap.encodeSettings)
        , SamplingEvent.fetch id |> Http.toTask |> Task.attempt GetSamplingEventCompleted
        , Sample.fetchAllBySamplingEvent id |> Http.toTask |> Task.attempt GetSamplesCompleted
        , SamplingEvent.fetchData id "niskin" |> Http.toTask |> Task.attempt GetNiskinDataCompleted
        , SamplingEvent.fetchData id "ctd" |> Http.toTask |> Task.attempt GetCTDDataCompleted
        ]
    )


toSession : Model -> Session
toSession model =
    model.session


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Workaround for race condition between view and Sample.fetch causing map creation to fail on missing gmap element
    Sub.batch
        [ Time.every 100 TimerTick -- milliseconds
        , GMap.mapLoaded MapLoaded
        ]



-- UPDATE --


type Msg
    = GetSamplingEventCompleted (Result Http.Error SamplingEvent)
    | GetSamplesCompleted (Result Http.Error (List Sample))
    | GetNiskinDataCompleted (Result Http.Error Data)
    | GetCTDDataCompleted (Result Http.Error Data)
    | MapLoaded Bool
    | TimerTick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetSamplingEventCompleted (Ok samplingEvent) ->
            ( { model | samplingEvent = Just samplingEvent }, Cmd.none )

        GetSamplingEventCompleted (Err error) -> --TODO
--            let
--                _ = Debug.log "GetSamplingEventCompleted" (toString error)
--            in
            ( model, Cmd.none )

        GetSamplesCompleted (Ok samples) ->
            ( { model | samples = Just samples }, Cmd.none )

        GetSamplesCompleted (Err error) -> --TODO
--            let
--                _ = Debug.log "GetSamplesCompleted" (toString error)
--            in
            ( model, Cmd.none )

        GetNiskinDataCompleted (Ok data) ->
            ( { model | niskinData = Success data }, Cmd.none )

        GetNiskinDataCompleted (Err error) ->
            --let
            --    _ = Debug.log "GetNiskinDataCompleted" (toString error)
            --in
            ( { model | niskinData = Failure error }, Cmd.none )

        GetCTDDataCompleted (Ok data) ->
            ( { model | ctdData = Success data }, Cmd.none )

        GetCTDDataCompleted (Err error) ->
            --let
            --    _ = Debug.log "GetCTDDataCompleted" (toString error)
            --in
            ( { model | ctdData = Failure error }, Cmd.none )

        MapLoaded success ->
            ( { model | mapLoaded = success }, Cmd.none )

        TimerTick time ->
            case (model.mapLoaded, model.samplingEvent) of
                (False, Just samplingEvent) ->
                    let
                        map =
                            samplingEvent.locations |> Encode.list LatLng.encode
                    in
                    ( model, GMap.loadMap map )

                (_, _) ->
                    ( model, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    case model.samplingEvent of
        Nothing ->
            text ""

        Just samplingEvent ->
            let
                pageTitle =
                    "Sampling Event (" ++ (String.Extra.toSentenceCase samplingEvent.type_) ++ ")"

                numSamples =
                    model.samples |> Maybe.map List.length |> Maybe.withDefault 0
            in
            div [ class "container" ]
                [ Page.viewTitle pageTitle samplingEvent.name
                , div []
                    [ viewSamplingEvent samplingEvent ]
                , div [ class "pt-3" ]
                    [ Page.viewTitle2 "Samples" False
                    , span [ class "badge badge-pill badge-primary align-middle ml-2" ]
                        [ if numSamples == 0 then
                            text ""
                          else
                            text (String.fromInt numSamples)
                        ]
                    ]
                , div [ class "pt-2" ]
                    [ viewSamples model.samples ]
                , div [ class "pt-3 pb-2" ]
                    [ Page.viewTitle2 "Niskin Data" False ]
                , viewData model.niskinData
                , div [ class "pt-3 pb-2" ]
                    [ Page.viewTitle2 "CTD Data" False ]
                , viewData model.ctdData
                ]


viewSamplingEvent : SamplingEvent -> Html Msg
viewSamplingEvent samplingEvent =
    let
        campaignRow =
            if samplingEvent.campaignId == 0 then
                tr []
                    [ th [] [ text "Campaign" ]
                    , td [] [ text "None" ]
                    ]
            else
                tr []
                    [ th [] [ text "Campaign (", text (String.Extra.toSentenceCase samplingEvent.campaignType), text ")" ]
                    , td [] [ a [ Route.href (Route.Campaign samplingEvent.campaignId) ] [ text samplingEvent.campaignName ] ]
                    ]
    in
    table [] -- ugh, use table for layout
        [ tr []
            [ td [ style "min-width" "50vw" ]
                [ table [ class "table table-borderless table-sm" ]
                    [ tbody []
                        [ tr []
                            [ th [ class "w-25" ] [ text "Name/ID" ]
                            , td [] [ text samplingEvent.name ]
                            ]
                        , tr []
                            [ th [] [ text "Type" ]
                            , td [] [ text (String.Extra.toSentenceCase samplingEvent.type_) ]
                            ]
                        , tr []
                            [ th [] [ text "Project" ]
                            , td [] [ a [ Route.href (Route.Project samplingEvent.projectId) ] [ text samplingEvent.projectName ] ]
                            ]
                        , campaignRow
                        , tr []
                            [ th [] [ text "Lat/Lng (deg)" ]
                            , td [] [ text (LatLng.formatList samplingEvent.locations) ]
                            ]
                        , tr []
                            [ th [] [ text "Start Time" ]
                            , td [] [ text samplingEvent.startTime ]
                            ]
                        , tr []
                            [ th [] [ text "End Time" ]
                            , td [] [ text samplingEvent.endTime ]
                            ]
                        ]
                    ]
                ]
            , td []
                [ viewMap ]
            ]
        ]


viewMap : Html Msg
viewMap =
    GMap.view [ class "border", style "display" "block", style "width" "20em", style "height" "12em" ] []


viewSamples : Maybe (List Sample) -> Html Msg
viewSamples maybeSamples =
    let
        mkRow sample =
            tr []
                [ td [ class "text-nowrap" ]
                    [ a [ Route.href (Route.Sample sample.id) ] [ text sample.accn ] ]
                ]
    in
    case maybeSamples |> Maybe.withDefault [] of
        [] ->
            text "None"

        samples ->
            table [ class "table" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Accn" ]
                        ]
                    ]
                , tbody []
                    (samples |> List.sortBy .accn |> List.map mkRow)
                ]


viewData : RemoteData Http.Error Data -> Html Msg
viewData maybeData =
    case maybeData of
        Success data ->
            let
                valueToString maybeValue =
                    case maybeValue of
                        Nothing ->
                            ""

                        Just (SearchTerm.StringValue v) ->
                            v

                        Just (SearchTerm.IntValue i) ->
                            String.fromInt i

                        Just (SearchTerm.FloatValue f) ->
                            String.fromFloat f

                mkRdf term =
                    if term.id /= "" then
                        let
                            purl =
                                -- PMO draft purls do not link to a human readable page, redirect them to the owl file GitHub (per Kai)
                                if String.startsWith "http://purl.obolibrary.org/obo/PMO" term.id then
                                    "https://raw.githubusercontent.com/hurwitzlab/planet-microbe-ontology/master/src/ontology/pmo-edit.owl"
                                else
                                    term.id
                        in
                        a [ id term.id, href purl, target "_blank" ] --, onMouseEnter (ShowTooltip term.id), onMouseLeave HideTooltip ]
                            [ text term.label ]
                    else
                        text term.alias_

                mkUnit term =
                    if term.unitId /= "" then
                        a [ href term.unitId, title term.unitId, target "_blank" ]
                            [ text term.unitLabel ]
                    else
                        text ""

                mkHeader terms =
                    tr []
                        (terms |> List.map
                            (\term ->
                                th []
                                    ((mkRdf term) ::
                                        (if term.unitId /= "" then
                                            [ text " (", (mkUnit term), text ")" ]
                                        else
                                            []
                                        )
                                    )
                            )
                        )

                mkRow values =
                    tr []
                        (values |> List.map (\val -> td [] [ val |> valueToString |> SearchTerm.viewValue ]))
            in
            div []
                [ table [ class "table table-sm table-striped small" ]
                    [ thead []
                        [ (mkHeader data.terms) ]
                    , tbody []
                        (List.map mkRow data.values)
                    ]
                ]

        Loading ->
            Page.viewSpinner

        _ ->
            text "None"