module Page.SamplingEvent exposing (Model, Msg, init, toSession, subscriptions, update, view)

import Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page
import Route
import SamplingEvent exposing (SamplingEvent)
import Sample exposing (Sample)
import LatLng
import GMap
import Time
import Http
--import Page.Error as Error exposing (PageLoadError)
import Task exposing (Task)
import String.Extra
import Json.Encode as Encode
--import Debug exposing (toString)



---- MODEL ----


type alias Model =
    { session : Session
    , samplingEvent : Maybe SamplingEvent
    , samples : Maybe (List Sample)
    , mapLoaded : Bool
    }


init : Session -> Int -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , samplingEvent = Nothing
      , samples = Nothing
      , mapLoaded = False
      }
      , Cmd.batch
        [ GMap.removeMap "" -- workaround for blank map on navigating back to this page
        , GMap.changeMapSettings (GMap.Settings False False True |> GMap.encodeSettings)
        , SamplingEvent.fetch id |> Http.toTask |> Task.attempt GetSamplingEventCompleted
        , Sample.fetchAllBySamplingEvent id |> Http.toTask |> Task.attempt GetSamplesCompleted
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
                        , tr []
                            [ th [] [ text "Data File(s)" ]
                            , td [] [ em [] [ text "(coming soon)"] ]
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
                [ td [ style "white-space" "nowrap" ]
                    [ a [ Route.href (Route.Sample sample.id) ] [ text sample.accn ] ]
                ]

        sortByAccn a b =
            compare a.accn b.accn
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
                    (samples |> List.sortWith sortByAccn |> List.map mkRow)
                ]