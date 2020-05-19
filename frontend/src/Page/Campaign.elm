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
import RemoteData exposing (RemoteData(..))
import String.Extra



---- MODEL ----


type alias Model =
    { session : Session
    , campaign : RemoteData Http.Error Campaign
    , samplingEvents : RemoteData Http.Error (List SamplingEvent)
    , samples : RemoteData Http.Error (List Sample)
    }


init : Session -> Int -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , campaign = Loading
      , samplingEvents = Loading
      , samples = Loading
      }
      , Cmd.batch
        [ Campaign.fetch id |> Http.send GetCampaignCompleted
        , SamplingEvent.fetchAllByCampaign id |> Http.send GetSamplingEventsCompleted
        , Sample.fetchAllByCampaign id |> Http.send GetSamplesCompleted
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
        GetCampaignCompleted result ->
            ( { model | campaign = RemoteData.fromResult result }, Cmd.none )

        GetSamplingEventsCompleted result ->
            ( { model | samplingEvents = RemoteData.fromResult result }, Cmd.none )

        GetSamplesCompleted result ->
            ( { model | samples = RemoteData.fromResult result }, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    Page.viewRemoteData model.campaign
        (\campaign ->
            let
                pageTitle =
                    "Campaign (" ++ (String.Extra.toSentenceCase campaign.type_) ++ ")"

                numSamplingEvents =
                    model.samplingEvents |> RemoteData.toMaybe |> Maybe.map List.length |> Maybe.withDefault 0

                numSamples =
                    model.samples |> RemoteData.toMaybe |> Maybe.map List.length |> Maybe.withDefault 0
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
        )


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


viewSamplingEvents : RemoteData Http.Error (List SamplingEvent) -> Html Msg
viewSamplingEvents samplingEvents =
    let
        mkRow samplingEvent =
            tr []
                [ td [ class "text-nowrap" ]
                    [ a [ Route.href (Route.SamplingEvent samplingEvent.id) ] [ text samplingEvent.name ] ]
                , td [] [ text (String.Extra.toSentenceCase samplingEvent.type_) ]
                ]
    in
    Page.viewRemoteData samplingEvents
        (\events ->
            table [ class "table" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Name/ID" ]
                        , th [] [ text "Type" ]
                        ]
                    ]
                , tbody []
                    (events |> List.sortBy .name |> List.map mkRow)
                ]
        )


viewSamples : RemoteData Http.Error (List Sample) -> Html Msg
viewSamples samples =
    let
        mkRow sample =
            tr []
                [ td [ class "text-nowrap" ]
                    [ a [ Route.href (Route.Sample sample.id) ] [ text sample.accn ] ]
                ]
    in
    Page.viewRemoteData samples
        (\s ->
            table [ class "table" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Accn" ]
                        ]
                    ]
                , tbody []
                    (s |> List.sortBy .accn |> List.map mkRow)
                ]
        )
