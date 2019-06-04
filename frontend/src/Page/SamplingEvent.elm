module Page.SamplingEvent exposing (Model, Msg, init, toSession, update, view)

import Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page
import Route
import SamplingEvent exposing (SamplingEvent)
import Sample exposing (Sample)
import LatLng
import Http
--import Page.Error as Error exposing (PageLoadError)
import Task exposing (Task)
import String.Extra
import Debug exposing (toString)



---- MODEL ----


type alias Model =
    { session : Session
    , samplingEvent : Maybe SamplingEvent
    , samples : Maybe (List Sample)
    }


init : Session -> Int -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , samplingEvent = Nothing
      , samples = Nothing
      }
      , Cmd.batch
        [ SamplingEvent.fetch id |> Http.toTask |> Task.attempt GetSamplingEventCompleted
        , Sample.fetchAllBySamplingEvent id |> Http.toTask |> Task.attempt GetSamplesCompleted
        ]
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE --


type Msg
    = GetSamplingEventCompleted (Result Http.Error SamplingEvent)
    | GetSamplesCompleted (Result Http.Error (List Sample))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetSamplingEventCompleted (Ok samplingEvent) ->
            ( { model | samplingEvent = Just samplingEvent }, Cmd.none )

        GetSamplingEventCompleted (Err error) -> --TODO
            let
                _ = Debug.log "GetSamplingEventCompleted" (toString error)
            in
            ( model, Cmd.none )

        GetSamplesCompleted (Ok samples) ->
            ( { model | samples = Just samples }, Cmd.none )

        GetSamplesCompleted (Err error) -> --TODO
            let
                _ = Debug.log "GetSamplesCompleted" (toString error)
            in
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
                            text (toString numSamples)
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
    table [ class "table table-borderless table-sm" ]
        [ tbody []
            [ tr []
                [ th [ class "w-25" ] [ text "Name/ID" ]
                , td [] [ text samplingEvent.name ]
                ]
            , campaignRow
            , tr []
                [ th [] [ text "Location(s)" ]
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