module Page.Campaign exposing (Model, Msg, init, toSession, update, view)

import Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page
import Route
import Campaign exposing (Campaign)
import SamplingEvent exposing (SamplingEvent)
import Sample exposing (Sample)
import Http
--import Page.Error as Error exposing (PageLoadError)
import Task exposing (Task)
import String.Extra
--import Debug exposing (toString)



---- MODEL ----


type alias Model =
    { session : Session
    , campaign : Maybe Campaign
    , samplingEvents : Maybe (List SamplingEvent)
    , samples : Maybe (List Sample)
    }


init : Session -> Int -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , campaign = Nothing
      , samplingEvents = Nothing
      , samples = Nothing
      }
      , Cmd.batch
        [ Campaign.fetch id |> Http.toTask |> Task.attempt GetCampaignCompleted
        , SamplingEvent.fetchAllByCampaign id |> Http.toTask |> Task.attempt GetSamplingEventsCompleted
        , Sample.fetchAllByCampaign id |> Http.toTask |> Task.attempt GetSamplesCompleted
        ]
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE --


type Msg
    = GetCampaignCompleted (Result Http.Error Campaign)
    | GetSamplingEventsCompleted (Result Http.Error (List SamplingEvent))
    | GetSamplesCompleted (Result Http.Error (List Sample))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetCampaignCompleted (Ok campaign) ->
            ( { model | campaign = Just campaign }, Cmd.none )

        GetCampaignCompleted (Err error) -> --TODO
--            let
--                _ = Debug.log "GetCampaignCompleted" (toString error)
--            in
            ( model, Cmd.none )

        GetSamplingEventsCompleted (Ok samplingEvents) ->
            ( { model | samplingEvents = Just samplingEvents }, Cmd.none )

        GetSamplingEventsCompleted (Err error) -> --TODO
--            let
--                _ = Debug.log "GetSamplingEventsCompleted" (toString error)
--            in
            ( model, Cmd.none )

        GetSamplesCompleted (Ok samples) ->
            ( { model | samples = Just samples }, Cmd.none )

        GetSamplesCompleted (Err error) -> --TODO
--            let
--                _ = Debug.log "GetSamplesCompleted" (toString error)
--            in
            ( model, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    case model.campaign of
        Nothing ->
            text ""

        Just campaign ->
            let
                pageTitle =
                    "Campaign (" ++ (String.Extra.toSentenceCase campaign.type_) ++ ")"

                numSamplingEvents =
                    model.samplingEvents |> Maybe.map List.length |> Maybe.withDefault 0

                numSamples =
                    model.samples |> Maybe.map List.length |> Maybe.withDefault 0
            in
            div [ class "container" ]
                [ Page.viewTitle pageTitle campaign.name
                , div []
                    [ viewCampaign campaign ]
                , div [ class "pt-3" ]
                    [ Page.viewTitle2 "Sampling Events" False
                    , span [ class "badge badge-pill badge-primary align-middle ml-2" ]
                        [ if numSamplingEvents == 0 then
                            text ""
                          else
                            text (String.fromInt numSamplingEvents)
                        ]
                    ]
                , div [ class "pt-2" ]
                    [ viewSamplingEvents model.samplingEvents ]
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


viewCampaign : Campaign -> Html Msg
viewCampaign campaign =
    table [ class "table table-borderless table-sm" ]
        [ tbody []
            [ tr []
                [ th [ class "w-25" ] [ text "Name" ]
                , td [] [ text campaign.name ]
                ]
            , tr []
                [ th [] [ text "Type" ]
                , td [] [ text (String.Extra.toSentenceCase campaign.type_) ]
                ]
            , tr []
                [ th [] [ text "Project" ]
                , td [] [ a [ Route.href (Route.Project campaign.projectId) ] [ text campaign.projectName ] ]
                ]
            , tr []
                [ th [] [ text "Description" ]
                , td [] [ text campaign.description ]
                ]
            , tr []
                [ th [] [ text "Deployment" ]
                , td [] [ text campaign.deployment ]
                ]
            , tr []
                [ th [] [ text "Start Location" ]
                , td [] [ text campaign.startLocation ]
                ]
            , tr []
                [ th [] [ text "End Location" ]
                , td [] [ text campaign.endLocation ]
                ]
            , tr []
                [ th [] [ text "Start Time" ]
                , td [] [ text campaign.startTime ]
                ]
            , tr []
                [ th [] [ text "End Time" ]
                , td [] [ text campaign.endTime ]
                ]
            , tr []
                [ th [] [ text "References" ]
                , td []
                    (List.map (\url -> div [] [ a [ href url, target "_blank" ] [ text url ] ]) campaign.urls)
                ]
            ]
        ]


viewSamplingEvents : Maybe (List SamplingEvent) -> Html Msg
viewSamplingEvents maybeSamplingEvents =
    let
        mkRow samplingEvent =
            tr []
                [ td [ style "white-space" "nowrap" ]
                    [ a [ Route.href (Route.SamplingEvent samplingEvent.id) ] [ text samplingEvent.name ] ]
                , td [] [ text (String.Extra.toSentenceCase samplingEvent.type_) ]
                ]
    in
    case maybeSamplingEvents |> Maybe.withDefault [] of
        [] ->
            text "None"

        samplingEvents ->
            table [ class "table" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Name/ID" ]
                        , th [] [ text "Type" ]
                        ]
                    ]
                , tbody []
                    (samplingEvents |> List.sortBy .name |> List.map mkRow)
                ]


viewSamples : Maybe (List Sample) -> Html Msg
viewSamples maybeSamples =
    let
        mkRow sample =
            tr []
                [ td [ style "white-space" "nowrap" ]
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
