module Page.Project exposing (Model, Msg, init, toSession, update, view)

import Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page
import Error
import Icon
import Project exposing (Project)
import Sample exposing (Sample)
import Campaign exposing (Campaign)
import SamplingEvent exposing (SamplingEvent)
import Route
import Http
import RemoteData exposing (RemoteData(..))
import String.Extra
import Config exposing (dataCommonsUrl)



---- MODEL ----


type alias Model =
    { session : Session
    , project : RemoteData Http.Error Project
    , campaigns : RemoteData Http.Error (List Campaign)
    , samplingEvents : RemoteData Http.Error (List SamplingEvent)
    , samples : RemoteData Http.Error (List Sample)
    }


init : Session -> Int -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , project = Loading
      , campaigns = Loading
      , samplingEvents = Loading
      , samples = Loading
      }
      , Cmd.batch
        [ Project.fetch id |> Http.send GetProjectCompleted
        , Campaign.fetchAllByProject id |> Http.send GetCampaignsCompleted
        , SamplingEvent.fetchAllByProject id |> Http.send GetSamplingEventsCompleted
        , Sample.fetchAllByProject id |> Http.send GetSamplesCompleted
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
            ( { model | project = Success project }, Cmd.none )

        GetProjectCompleted (Err error) ->
            ( { model | project = Failure error }, Cmd.none )

        GetCampaignsCompleted (Ok campaigns) ->
            ( { model | campaigns = Success campaigns }, Cmd.none )

        GetCampaignsCompleted (Err error) ->
            ( { model | campaigns = Failure error }, Cmd.none )

        GetSamplingEventsCompleted (Ok samplingEvents) ->
            ( { model | samplingEvents = Success samplingEvents }, Cmd.none )

        GetSamplingEventsCompleted (Err error) ->
            ( { model | samplingEvents = Failure error }, Cmd.none )

        GetSamplesCompleted (Ok samples) ->
            ( { model | samples = Success samples }, Cmd.none )

        GetSamplesCompleted (Err error) ->
            ( { model | samples = Failure error }, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    case model.project of
        Success project ->
            let
                numCampaigns =
                    model.campaigns |> RemoteData.toMaybe |> Maybe.map List.length |> Maybe.withDefault 0

                numSamplingEvents =
                    model.samplingEvents |> RemoteData.toMaybe |> Maybe.map List.length |> Maybe.withDefault 0

                numSamples =
                    model.samples |> RemoteData.toMaybe |> Maybe.map List.length |> Maybe.withDefault 0
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
                            text (String.fromInt numCampaigns)
                        ]
                    ]
                , div [ class "pt-2", style "overflow-y" "auto", style "max-height" "80vh" ]
                    [ viewRemoteData viewCampaigns model.campaigns ]
                , div [ class "pt-4" ]
                    [ Page.viewTitle2 "Sampling Events" False
                    , span [ class "badge badge-pill badge-primary align-middle ml-2" ]
                        [ if numSamplingEvents == 0 then
                            text ""
                          else
                            text (String.fromInt numSamplingEvents)
                        ]
                    ]
                , div [ class "pt-2", style "overflow-y" "auto", style "max-height" "80vh" ]
                    [ viewRemoteData viewSamplingEvents model.samplingEvents ]
                , div [ class "pt-4" ]
                    [ Page.viewTitle2 "Samples" False
                    , span [ class "badge badge-pill badge-primary align-middle ml-2" ]
                        [ if numSamples == 0 then
                            text ""
                          else
                            text (String.fromInt numSamples)
                        ]
                    ]
                , div [ class "pt-2", style "overflow-y" "auto", style "max-height" "80vh" ]
                    [ viewRemoteData viewSamples model.samples ]
                ]

        Failure error ->
            Error.view error False

        _ ->
            Page.viewSpinnerOverlay


viewProject : Project -> Html Msg
viewProject project =
    let
        fileTable files =
            table [ class "table table-borderless table-sm small" ]
                [ tbody []
                    (files |> List.sortBy .url |> List.map fileRow)
                ]

        fileRow f =
            tr []
                [ td [] [ text (String.Extra.toSentenceCase f.type_) ]
                , td [] [ a [ href (dataCommonsUrl ++ f.url), target "_blank" ] [ text f.url ] ]
                ]
    in
    table [ class "table table-borderless table-sm" ]
        [ tbody []
            [ tr []
                [ th [ class "w-25" ] [ text "Name" ]
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
            , tr []
                [ th [] [ text "URL ", Icon.externalLink ]
                , td [] [ a [ href project.url, target "_blank" ] [ text project.url ] ]
                ]
            , tr []
                [ th [] [ text "Data Package ", Icon.externalLink ]
                , td [] [ a [ href project.datapackageUrl, target "_blank" ] [ text project.datapackageUrl ] ]
                ]
            , tr []
                [ th [] [ text "Files" ]
                , td []
                    [ if project.files /= [] then
                        fileTable project.files
                      else
                        text "None"
                    ]
                ]
            ]
        ]


viewRemoteData : (List a -> Html msg) -> RemoteData Http.Error (List a) -> Html msg
viewRemoteData viewFunc remoteData =
    case remoteData of
        Success data ->
            viewFunc data

        Failure error ->
            Error.view error False

        _ ->
            Page.viewSpinnerOverlay


viewCampaigns : List Campaign -> Html Msg
viewCampaigns campaigns =
    let
        mkRow campaign =
            tr []
                [ td [ class "text-nowrap" ]
                    [ a [ Route.href (Route.Campaign campaign.id) ] [ text campaign.name ] ]
                , td [] [ text (String.Extra.toSentenceCase campaign.type_) ]
                ]
    in
    table [ class "table" ]
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Type" ]
                ]
            ]
        , tbody []
            (campaigns |> List.sortBy .name |> List.map mkRow)
        ]


viewSamplingEvents : List SamplingEvent -> Html Msg
viewSamplingEvents samplingEvents =
    let
        mkRow samplingEvent =
            tr []
                [ td [ class "text-nowrap" ]
                    [ a [ Route.href (Route.SamplingEvent samplingEvent.id) ] [ text samplingEvent.name ] ]
                , td [] [ text (String.Extra.toSentenceCase samplingEvent.type_) ]
                ]
    in
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


viewSamples : List Sample -> Html Msg
viewSamples samples =
    let
        mkRow sample =
            tr []
                [ td [ class "text-nowrap" ]
                    [ a [ Route.href (Route.Sample sample.id) ] [ text sample.accn ] ]
                ]
    in
    table [ class "table" ]
        [ thead []
            [ tr []
                [ th [] [ text "Accn" ]
                ]
            ]
        , tbody []
            (samples |> List.sortBy .accn |> List.map mkRow)
        ]