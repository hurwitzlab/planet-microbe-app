module Page.Project exposing (Model, Msg, init, toSession, update, view)

import Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page
import Project exposing (Project)
import Sample exposing (Sample)
import Campaign exposing (Campaign)
import SamplingEvent exposing (SamplingEvent)
import Route
import Http
--import Page.Error as Error exposing (PageLoadError)
import Task exposing (Task)
import String.Extra
import Debug exposing (toString)



---- MODEL ----


type alias Model =
    { session : Session
    , project : Maybe Project
    , campaigns : Maybe (List Campaign)
    , samplingEvents : Maybe (List SamplingEvent)
    , samples : Maybe (List Sample)
    }


init : Session -> Int -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , project = Nothing
      , campaigns = Nothing
      , samplingEvents = Nothing
      , samples = Nothing
      }
      , Cmd.batch
        [ Project.fetch id |> Http.toTask |> Task.attempt GetProjectCompleted
        , Campaign.fetchAllByProject id |> Http.toTask |> Task.attempt GetCampaignsCompleted
        , SamplingEvent.fetchAllByProject id |> Http.toTask |> Task.attempt GetSamplingEventsCompleted
        , Sample.fetchAllByProject id |> Http.toTask |> Task.attempt GetSamplesCompleted
        ]
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE --


type Msg
    = GetProjectCompleted (Result Http.Error Project)
    | GetCampaignsCompleted (Result Http.Error (List Campaign))
    | GetSamplingEventsCompleted (Result Http.Error (List SamplingEvent))
    | GetSamplesCompleted (Result Http.Error (List Sample))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetProjectCompleted (Ok project) ->
            ( { model | project = Just project }, Cmd.none )

        GetProjectCompleted (Err error) -> --TODO
            let
                _ = Debug.log "GetProjectCompleted" (toString error)
            in
            ( model, Cmd.none )

        GetCampaignsCompleted (Ok campaigns) ->
            ( { model | campaigns = Just campaigns }, Cmd.none )

        GetCampaignsCompleted (Err error) -> --TODO
            let
                _ = Debug.log "GetCampaignsCompleted" (toString error)
            in
            ( model, Cmd.none )

        GetSamplingEventsCompleted (Ok samplingEvents) ->
            ( { model | samplingEvents = Just samplingEvents }, Cmd.none )

        GetSamplingEventsCompleted (Err error) -> --TODO
            let
                _ = Debug.log "GetSamplingEventsCompleted" (toString error)
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
    case model.project of
        Nothing ->
            text ""

        Just project ->
            let
                numCampaigns =
                    model.campaigns |> Maybe.map List.length |> Maybe.withDefault 0

                numSamplingEvents =
                    model.samplingEvents |> Maybe.map List.length |> Maybe.withDefault 0

                numSamples =
                    model.samples |> Maybe.map List.length |> Maybe.withDefault 0
            in
            div [ class "container" ]
                [ div []
                    [ Page.viewTitle "Project" project.name ]
                , div []
                    [ viewProject project ]
                , div [ class "pt-3" ]
                    [ Page.viewTitle2 "Campaigns" False
                    , span [ class "badge badge-pill badge-primary align-middle ml-2" ]
                        [ if numCampaigns == 0 then
                            text ""
                          else
                            text (toString numCampaigns)
                        ]
                    ]
                , div [ class "pt-2", style "overflow-y" "auto", style "max-height" "80vh" ]
                    [ viewCampaigns model.campaigns ]
                , div [ class "pt-4" ]
                    [ Page.viewTitle2 "Sampling Events" False
                    , span [ class "badge badge-pill badge-primary align-middle ml-2" ]
                        [ if numSamplingEvents == 0 then
                            text ""
                          else
                            text (toString numSamplingEvents)
                        ]
                    ]
                , div [ class "pt-2", style "overflow-y" "auto", style "max-height" "80vh" ]
                    [ viewSamplingEvents model.samplingEvents ]
                , div [ class "pt-4" ]
                    [ Page.viewTitle2 "Samples" False
                    , span [ class "badge badge-pill badge-primary align-middle ml-2" ]
                        [ if numSamples == 0 then
                            text ""
                          else
                            text (toString numSamples)
                        ]
                    ]
                , div [ class "pt-2", style "overflow-y" "auto", style "max-height" "80vh" ]
                    [ viewSamples model.samples ]
                ]


viewProject : Project -> Html Msg
viewProject project =
    table [ class "table table-borderless table-sm" ]
        [ tbody []
            [ tr []
                [ th [] [ text "Name" ]
                , td [] [ text project.name ]
                ]
            , tr []
                [ th [] [ text "Description" ]
                , td [] [ text project.description ]
                ]
            , tr []
                [ th [] [ text "Type" ]
                , td [] [ text (String.Extra.toSentenceCase project.type_) ]
                ]
            ]
        ]


viewCampaigns : Maybe (List Campaign) -> Html Msg
viewCampaigns maybeCampaigns =
    let
        mkRow campaign =
            tr []
                [ td [ style "white-space" "nowrap" ]
                    [ a [ Route.href (Route.Campaign campaign.id) ] [ text campaign.name ] ]
                ]

        sortByName a b =
            compare a.name b.name
    in
    case maybeCampaigns |> Maybe.withDefault [] of
        [] ->
            text "None"

        samples ->
            table [ class "table" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Name" ]
                        ]
                    ]
                , tbody []
                    (samples |> List.sortWith sortByName |> List.map mkRow)
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
    case maybeSamplingEvents |> Maybe.withDefault [] of
        [] ->
            text "None"

        samples ->
            table [ class "table" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Name/ID" ]
                        ]
                    ]
                , tbody []
                    (samples |> List.sortWith sortByName |> List.map mkRow)
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