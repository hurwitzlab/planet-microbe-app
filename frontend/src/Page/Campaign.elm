module Page.Campaign exposing (Model, Msg, init, toSession, update, view)

import Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page
import Route
import Campaign exposing (Campaign)
import SamplingEvent exposing (SamplingEvent)
import Http
--import Page.Error as Error exposing (PageLoadError)
import Task exposing (Task)
import String.Extra
import Debug exposing (toString)



---- MODEL ----


type alias Model =
    { session : Session
    , campaign : Maybe Campaign
    , samplingEvents : Maybe (List SamplingEvent)
    }


init : Session -> Int -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , campaign = Nothing
      , samplingEvents = Nothing
      }
      , Cmd.batch
        [ Campaign.fetch id |> Http.toTask |> Task.attempt GetCampaignCompleted
        , SamplingEvent.fetchAllByCampaign id |> Http.toTask |> Task.attempt GetSamplingEventsCompleted
        ]
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE --


type Msg
    = GetCampaignCompleted (Result Http.Error Campaign)
    | GetSamplingEventsCompleted (Result Http.Error (List SamplingEvent))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetCampaignCompleted (Ok campaign) ->
            ( { model | campaign = Just campaign }, Cmd.none )

        GetCampaignCompleted (Err error) -> --TODO
            let
                _ = Debug.log "GetCampaignCompleted" (toString error)
            in
            ( model, Cmd.none )

        GetSamplingEventsCompleted (Ok samplingEvents) ->
            ( { model | samplingEvents = Just samplingEvents }, Cmd.none )

        GetSamplingEventsCompleted (Err error) -> --TODO
            let
                _ = Debug.log "GetSamplingEventsCompleted" (toString error)
            in
            ( model, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    case model.campaign of
        Nothing ->
            text ""

        Just campaign ->
            let
                numSamplingEvents =
                    model.samplingEvents |> Maybe.map List.length |> Maybe.withDefault 0
            in
            div [ class "container" ]
                [ Page.viewTitle (String.Extra.toSentenceCase campaign.type_) campaign.name
                , div []
                    [ viewCampaign campaign ]
                , div [ class "pt-3" ]
                    [ Page.viewTitle2 "Sampling Events" False
                    , span [ class "badge badge-pill badge-primary align-middle ml-2" ]
                        [ if numSamplingEvents == 0 then
                            text ""
                          else
                            text (toString numSamplingEvents)
                        ]
                    ]
                , div [ class "pt-2" ]
                    [ viewSamplingEvents model.samplingEvents ]
                ]


viewCampaign : Campaign -> Html Msg
viewCampaign campaign =
    table [ class "table table-borderless table-sm" ]
        [ tbody []
            [ tr []
                [ th [] [ text "Name" ]
                , td [] [ text campaign.name ]
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
                , td [] (List.map (\url -> div [] [ a [ href url, target "_blank" ] [ text url ] ]) campaign.urls)
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
                ]

        sortByName a b =
            compare a.name b.name
    in
    case maybeSamplingEvents of
        Nothing ->
            text "None"

        Just samplingEvents ->
            table [ class "table" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Name/ID" ]
                        ]
                    ]
                , tbody []
                    (samplingEvents |> List.sortWith sortByName |> List.map mkRow)
                ]