module Page.Sample exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Session exposing (Session)
import Browser.Dom exposing (Error(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onMouseEnter, onMouseLeave)
import Page
import Route
import Sample exposing (Sample, Metadata, Value(..), SearchTerm, Annotation)
import SamplingEvent exposing (SamplingEvent)
import LatLng
import GMap
import Http
--import Page.Error as Error exposing (PageLoadError)
import Task exposing (Task)
import Time
import String.Extra
import List.Extra
import Json.Encode as Encode
--import Debug exposing (toString)



---- MODEL ----


type alias Model =
    { session : Session
    , sample : Maybe Sample
    , samplingEvents : Maybe (List SamplingEvent)
    , terms : Maybe (List SearchTerm)
    , metadata : Maybe Metadata
    , mapLoaded : Bool
    , tooltip : Maybe (ToolTip (List Annotation))
    }


type alias ToolTip a = --TODO move tooltip code into own module
    { x : Float
    , y : Float
    , content : a
    }


init : Session -> Int -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , sample = Nothing
      , samplingEvents = Nothing
      , terms = Nothing
      , metadata = Nothing
      , mapLoaded = False
      , tooltip = Nothing
      }
      , Cmd.batch
        [ GMap.removeMap "" -- workaround for blank map on navigating back to this page
        , GMap.changeMapSettings (GMap.Settings False False True False |> GMap.encodeSettings)
        , Sample.fetch id |> Http.toTask |> Task.attempt GetSampleCompleted
        , SamplingEvent.fetchAllBySample id |> Http.toTask |> Task.attempt GetSamplingEventsCompleted
        , Sample.fetchSearchTerms |> Http.toTask |> Task.attempt GetSearchTermsCompleted
        , Sample.fetchMetadata id |> Http.toTask |> Task.attempt GetMetadataCompleted
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
    = GetSampleCompleted (Result Http.Error Sample)
    | GetSamplingEventsCompleted (Result Http.Error (List SamplingEvent))
    | GetSearchTermsCompleted (Result Http.Error (List SearchTerm))
    | GetMetadataCompleted (Result Http.Error Metadata)
    | MapLoaded Bool
    | TimerTick Time.Posix
    | ShowTooltip String
    | HideTooltip
    | GotElement (Result Browser.Dom.Error Browser.Dom.Element)
    | GetSearchTermCompleted (Result Http.Error SearchTerm)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetSampleCompleted (Ok sample) ->
            ( { model | sample = Just sample }, Cmd.none )

        GetSampleCompleted (Err error) -> --TODO
--            let
--                _ = Debug.log "GetSampleCompleted" (toString error)
--            in
            ( model, Cmd.none )

        GetSamplingEventsCompleted (Ok samplingEvents) ->
            ( { model | samplingEvents = Just samplingEvents }, Cmd.none )

        GetSamplingEventsCompleted (Err error) -> --TODO
--            let
--                _ = Debug.log "GetSamplingEventsCompleted" (toString error)
--            in
            ( model, Cmd.none )

        GetSearchTermsCompleted (Ok terms) ->
            ( { model | terms = Just terms }, Cmd.none )

        GetSearchTermsCompleted (Err error) -> --TODO
--            let
--                _ = Debug.log "GetSearchTermsCompleted" (toString error)
--            in
            ( model, Cmd.none )

        GetMetadataCompleted (Ok metadata) ->
            ( { model | metadata = Just metadata }, Cmd.none )

        GetMetadataCompleted (Err error) -> --TODO
--            let
--                _ = Debug.log "GetMetadataCompleted" (toString error)
--            in
            ( model, Cmd.none )

        MapLoaded success ->
            ( { model | mapLoaded = success }, Cmd.none )

        TimerTick time ->
            case (model.mapLoaded, model.sample) of
                (False, Just sample) ->
                    let
                        map =
                            sample.locations |> Encode.list LatLng.encode
                    in
                    ( model, GMap.loadMap map )

                (_, _) ->
                    ( model, Cmd.none )

        ShowTooltip purl ->
            let
                getElement =
                    Browser.Dom.getElement purl |> Task.attempt GotElement

                getSearchTerm =
                    Sample.fetchSearchTerm purl |> Http.toTask |> Task.attempt GetSearchTermCompleted
            in
            ( model, Cmd.batch [ getElement, getSearchTerm ] )

        HideTooltip ->
            ( { model | tooltip = Nothing }, Cmd.none )

        GotElement (Ok element) ->
            let
                x =
                    element.element.x + element.element.width + 10

                y =
                    element.element.y - 10
            in
            case model.tooltip of
                Just tooltip ->
                    ( { model | tooltip = Just ( { tooltip | x = x, y = y } ) }, Cmd.none )

                Nothing ->
                    ( { model | tooltip = Just (ToolTip x y []) }, Cmd.none )

        GotElement (Err error) ->
--            let
--                _ = Debug.log "GotElement" (toString error)
--            in
            ( model, Cmd.none )

        GetSearchTermCompleted (Ok term) ->
            let
                purlsToHide =
                    [ "http://purl.obolibrary.org/obo/IAO_0000116" -- editor's note
                    ]

                filtered =
                    term.annotations
                        |> List.filter (\a -> not (List.member a.id purlsToHide))

                newTooltip =
                    case model.tooltip of
                        Just tooltip ->
                            Just { tooltip | content = filtered }

                        Nothing ->
                            Just (ToolTip 0 0 term.annotations)
            in
            ( { model | tooltip = newTooltip }, Cmd.none )

        GetSearchTermCompleted (Err error) -> --TODO
--            let
--                _ = Debug.log "GetSearchTermCompleted" (toString error)
--            in
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
                    [ viewSample sample (model.samplingEvents |> Maybe.withDefault []) ]
                , div [ class "pt-3 pb-2" ]
                    [ Page.viewTitle2 "Metadata" False ]
                , viewMetadata model.metadata model.terms
                , case model.tooltip of
                    Nothing ->
                        text ""

                    Just tooltip ->
                        viewTooltip tooltip
                ]


viewSample : Sample -> List SamplingEvent -> Html Msg
viewSample sample samplingEvents =
    let
        campaigns =
            samplingEvents
                |> List.filter (\event -> event.campaignId /= 0)
                |> List.map (\event -> (event.campaignId, event.campaignType, event.campaignName) )

        campaignsRow =
            tr []
                [ th [ class "text-nowrap" ] [ text "Campaigns" ]
                , if campaigns == [] then
                    td [] [ text "None" ]
                else
                    td []
                        (campaigns
                            |> List.Extra.unique
                            |> List.map
                                (\(id, type_, name) ->
                                    a [ Route.href (Route.Campaign id) ]
                                        [ (String.Extra.toSentenceCase type_) ++ " " ++ name |> text ]
                                )
                            |> List.intersperse (text ", ")
                        )
                ]

        samplingEventsRow =
            tr []
                [ th [ class "text-nowrap" ] [ text "Sampling Events" ]
                , if samplingEvents == [] then
                    td [] [ text "None" ]
                else
                    td []
                        (samplingEvents
                            |> List.map
                                (\event ->
                                    a [ Route.href (Route.SamplingEvent event.id) ]
                                        [ (String.Extra.toSentenceCase event.type_) ++ " " ++ event.name |> text ]
                                )
                            |> List.intersperse (text ", ")
                        )
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
                        , campaignsRow
                        , samplingEventsRow
                        , tr []
                            [ th [] [ text "Lat/Lng (deg)" ]
                            , td [] [ text (sample.locations |> LatLng.unique |> LatLng.formatList) ]
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
viewMetadata maybeMetadata maybeTerms  =
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
                            String.fromInt i

                        Just (FloatValue f) ->
                            String.fromFloat f

                getTermProperty id prop =
                    List.filter (\t -> t.id == id) terms |> List.map prop |> List.head

                mkRdf field =
                    if field.rdfType /= "" then
                        div []
                            [ a [ href field.rdfType, target "_blank", id field.rdfType ]
                                [ getTermProperty field.rdfType .label |> Maybe.withDefault "" |> text ]
                            ]
                    else
                        text ""

                mkUnitRdf field =
                    if field.unitRdfType /= "" then
                        a [ href field.unitRdfType, title field.unitRdfType, target "_blank" ]
                                [ getTermProperty field.rdfType .unitLabel |> Maybe.withDefault "" |> text ]
                    else
                        text ""

                mkSourceUrl url =
                    if url == "" then
                        text ""
                    else
                        a [ href url, target "_blank" ] [ text "Link" ]

                mkRow index (field, maybeValue) =
                    tr []
                        [ if field.rdfType /= "" then
                            td [ onMouseEnter (ShowTooltip field.rdfType), onMouseLeave HideTooltip ] [ mkRdf field ]
                          else
                            td [] [ mkRdf field ]
                        , td [] [ text field.name ]
                        , td [] [ maybeValue |> valueToString |> viewValue ]
                        , td [] [ mkUnitRdf field ]
                        , td [] [ mkSourceUrl field.sourceUrl ]
                        ]

                extLinkIcon =
                    i [ class "fas fa-external-link-alt fa-xs align-baseline ml-2" ] []
            in
            table [ class "table table-sm" ]
                [ thead []
                    [ tr []
                        [ th [ class "text-nowrap" ] [ text "ENVO Label", extLinkIcon ]
                        , th [ class "text-nowrap" ] [ text "Dataset Label" ]
                        , th [] [ text "Value" ]
                        , th [] [ text "Unit" ]
                        , th [ class "text-nowrap" ] [ text "Source", extLinkIcon ]
                        ]
                    ]
                , tbody []
                    (List.Extra.zip metadata.fields metadata.values |> List.indexedMap mkRow )
                ]

        _ ->
            text "None"


viewTooltip : ToolTip (List Annotation) -> Html msg
viewTooltip tooltip =
    if tooltip.content /= [] && tooltip.x /= 0 && tooltip.y /= 0 then
        let
            top =
                (String.fromFloat tooltip.y) ++ "px"

            left =
                (String.fromFloat tooltip.x) ++ "px"

            row anno =
                tr []
                    [ th [] [ text anno.label ]
                    , td [] [ text anno.value ]
                    ]
        in
        div [ class "rounded border py-2 px-3", style "background-color" "#efefef", style "z-index" "1000", style "position" "absolute", style "top" top, style "left" left ]
            [ table []
                (List.map row tooltip.content)
            ]
    else
        text ""


viewValue : String -> Html msg
viewValue val =
    if String.startsWith "http://" val || String.startsWith "https://" val || String.startsWith "ftp://" val then
        a [ href val, target "_blank" ] [ text val ]
    else
        text val
