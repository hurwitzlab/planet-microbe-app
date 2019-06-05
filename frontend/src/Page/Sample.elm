module Page.Sample exposing (Model, Msg, init, toSession, update, view)

import Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page
import Route
import Sample exposing (Sample, Metadata, Value(..), SearchTerm)
import LatLng
import GMap
import Http
--import Page.Error as Error exposing (PageLoadError)
import Task exposing (Task)
import String.Extra
import List.Extra
import Json.Encode as Encode
import Debug exposing (toString)



---- MODEL ----


type alias Model =
    { session : Session
    , sample : Maybe Sample
    , terms : Maybe (List SearchTerm)
    , metadata : Maybe Metadata
    }


init : Session -> Int -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , sample = Nothing
      , terms = Nothing
      , metadata = Nothing
      }
      , Cmd.batch
        [ GMap.removeMap "" -- workaround for blank map on navigating back to this page
        , GMap.changeMapSettings (GMap.Settings False False True |> GMap.encodeSettings)
        , Sample.fetch id |> Http.toTask |> Task.attempt GetSampleCompleted
        , Sample.fetchSearchTerms |> Http.toTask |> Task.attempt GetSearchTermsCompleted
        , Sample.fetchMetadata id |> Http.toTask |> Task.attempt GetMetadataCompleted
        ]
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE --


type Msg
    = GetSampleCompleted (Result Http.Error Sample)
    | GetSearchTermsCompleted (Result Http.Error (List SearchTerm))
    | GetMetadataCompleted (Result Http.Error Metadata)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetSampleCompleted (Ok sample) ->
            let
                map =
                    sample.locations |> Encode.list LatLng.encode
            in
            ( { model | sample = Just sample }, GMap.loadMap map )

        GetSampleCompleted (Err error) -> --TODO
            let
                _ = Debug.log "GetSampleCompleted" (toString error)
            in
            ( model, Cmd.none )

        GetSearchTermsCompleted (Ok terms) ->
            ( { model | terms = Just terms }, Cmd.none )

        GetSearchTermsCompleted (Err error) -> --TODO
            let
                _ = Debug.log "GetSearchTermsCompleted" (toString error)
            in
            ( model, Cmd.none )

        GetMetadataCompleted (Ok metadata) ->
            ( { model | metadata = Just metadata }, Cmd.none )

        GetMetadataCompleted (Err error) -> --TODO
            let
                _ = Debug.log "GetMetadataCompleted" (toString error)
            in
            ( model, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    case model.sample of
        Nothing ->
            text ""

        Just sample ->
            div [ class "container" ]
                [ Page.viewTitle "Sample" sample.accn
                , div []
                    [ viewSample sample ]
                , div [ class "pt-3 pb-2" ]
                    [ Page.viewTitle2 "Metadata" False ]
                , viewMetadata model.metadata model.terms
                ]


viewSample : Sample -> Html Msg
viewSample sample =
    let
        campaignRow =
            if sample.campaignId == 0 then
                tr []
                    [ th [] [ text "Campaign" ]
                    , td [] [ text "None" ]
                    ]
            else
                tr []
                    [ th [] [ text "Campaign (", text (String.Extra.toSentenceCase sample.campaignType), text ")" ]
                    , td [] [ a [ Route.href (Route.Campaign sample.campaignId) ] [ text sample.campaignName ] ]
                    ]

        samplingEventRow =
            if sample.samplingEventId == 0 then
                tr []
                    [ th [ class "text-nowrap" ] [ text "Sampling Event" ]
                    , td [] [ text "None" ]
                    ]
            else
                tr []
                    [ th [ class "text-nowrap" ] [ text "Sampling Event (", text (String.Extra.toSentenceCase sample.samplingEventType), text ")" ]
                    , td [] [ a [ Route.href (Route.SamplingEvent sample.samplingEventId) ] [ text sample.samplingEventName ] ]
                    ]
    in
    table [] -- ugh, use table for layout
        [ tr []
            [ td [ style "min-width" "50vw" ]
                [ table [ class "table table-borderless table-sm" ]
                    [ tbody []
                        [ tr []
                            [ th [ class "w-25" ] [ text "Accession" ]
                            , td [class "w-50"] [  a [ href ("https://www.ncbi.nlm.nih.gov/biosample/?term=" ++ sample.accn), target "_blank" ] [ text sample.accn ] ]
                            ]
                        , tr []
                            [ th [] [ text "Project" ]
                            , td [] [ a [ Route.href (Route.Project sample.projectId) ] [ text sample.projectName ] ]
                            ]
                        , campaignRow
                        , samplingEventRow
                        , tr []
                            [ th [] [ text "Lat/Lng (deg)" ]
                            , td [] [ text (LatLng.formatList sample.locations) ]
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


viewMetadata : Maybe Metadata -> Maybe (List SearchTerm) -> Html Msg
viewMetadata maybeMetadata maybeTerms =
    case (maybeMetadata, maybeTerms) of
        (Just metadata, Just terms) ->
            let
                valueToString maybeValue =
                    case maybeValue of
                        Nothing ->
                            ""

                        Just (StringValue v) ->
                            v

                        Just (IntValue i) ->
                            toString i

                        Just (FloatValue f) ->
                            toString f

                getTermProperty id prop =
                    List.filter (\t -> t.id == id) terms |> List.map prop |> List.head

                mkValue val =
                    if String.startsWith "http://" val || String.startsWith "https://" val || String.startsWith "ftp://" val then
                        a [ href val, target "_blank" ] [ text val ]
                    else
                        text val

                mkRow (field, maybeValue) =
                    tr []
                        [ td []
                            [ a [ href field.rdfType, title field.rdfType, target "_blank" ]
                                [ getTermProperty field.rdfType .label |> Maybe.withDefault "" |> text ]
                            ]
                        , td [] [ text field.name ]
                        , td [] [ maybeValue |> valueToString |> mkValue ]
                        , td []
                            [ a [ href field.unitRdfType, title field.unitRdfType, target "_blank" ]
                                [ getTermProperty field.rdfType .unitLabel |> Maybe.withDefault "" |> text ]
                            ]
                        ]
            in
            table [ class "table table-sm" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "ENVO Label" ]
                        , th [] [ text "Dataset Label" ]
                        , th [] [ text "Value" ]
                        , th [] [ text "Unit" ]
                        ]
                    ]
                , tbody []
                    (List.Extra.zip metadata.fields metadata.values |> List.map mkRow )
                ]

        _ ->
            text "None"


