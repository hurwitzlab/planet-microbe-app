module Page.Sample exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Session exposing (Session)
import Browser.Dom exposing (Error(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onMouseEnter, onMouseLeave, onClick)
import Page exposing (viewSpinner)
import Route
import Error
import Sample exposing (Sample, Metadata)
import Search exposing (PURL, Value(..), SearchTerm, Annotation)
import SamplingEvent exposing (SamplingEvent)
import Experiment exposing (Experiment)
import LatLng
import GMap
import Http
import RemoteData exposing (RemoteData(..))
import Task exposing (Task)
import Time
import String.Extra
import List.Extra
import Json.Encode as Encode
import Cart
import Icon
--import Debug exposing (toString)



---- MODEL ----


type alias Model =
    { session : Session
    , sample : RemoteData Http.Error Sample
    , samplingEvents : Maybe (List SamplingEvent)
    , experiments : Maybe (List Experiment)
    , metadata : Maybe Metadata
    , mapLoaded : Bool
    , tooltip : Maybe (ToolTip (List Annotation))
    , showUnannotatedMetadata : Bool
    }


type alias ToolTip a = --TODO move tooltip code into own module
    { x : Float
    , y : Float
    , content : a
    }


init : Session -> Int -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , sample = Loading
      , samplingEvents = Nothing
      , experiments = Nothing
      , metadata = Nothing
      , mapLoaded = False
      , tooltip = Nothing
      , showUnannotatedMetadata = False
      }
      , Cmd.batch
        [ GMap.removeMap "" -- workaround for blank map on navigating back to this page
        , GMap.changeMapSettings (GMap.Settings False False True False |> GMap.encodeSettings)
        , Sample.fetch id |> Http.send GetSampleCompleted
        , Sample.fetchMetadata id |> Http.send GetMetadataCompleted
        , SamplingEvent.fetchAllBySample id |> Http.send GetSamplingEventsCompleted
        , Experiment.fetchAllBySample id |> Http.send GetExperimentsCompleted
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
    | GetExperimentsCompleted (Result Http.Error (List Experiment))
    | GetMetadataCompleted (Result Http.Error Metadata)
    | MapLoaded Bool
    | TimerTick Time.Posix
    | ShowTooltip PURL
    | HideTooltip
    | GotElement PURL (Result Browser.Dom.Error Browser.Dom.Element)
    | ToggleUnannotatedMetadata
    | CartMsg Cart.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetSampleCompleted (Ok sample) ->
            ( { model | sample = Success sample }, Cmd.none )

        GetSampleCompleted (Err error) -> --TODO
--            let
--                _ = Debug.log "GetSampleCompleted" (toString error)
--            in
            ( { model | sample = Failure error }, Cmd.none )

        GetSamplingEventsCompleted (Ok samplingEvents) ->
            ( { model | samplingEvents = Just samplingEvents }, Cmd.none )

        GetSamplingEventsCompleted (Err error) -> --TODO
--            let
--                _ = Debug.log "GetSamplingEventsCompleted" (toString error)
--            in
            ( model, Cmd.none )

        GetExperimentsCompleted (Ok experiments) ->
            ( { model | experiments = Just experiments }, Cmd.none )

        GetExperimentsCompleted (Err error) -> --TODO
--            let
--                _ = Debug.log "GetExperimentsCompleted" (toString error)
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
                (False, Success sample) ->
                    let
                        map =
                            sample.locations |> Encode.list LatLng.encode
                    in
                    ( model, GMap.loadMap map )

                (_, _) ->
                    ( model, Cmd.none )

        ShowTooltip purl ->
            if purl /= "" then
                let
                    getElement =
                        Browser.Dom.getElement purl |> Task.attempt (GotElement purl)
                in
                ( model, getElement )
            else
                ( { model | tooltip = Nothing}, Cmd.none )

        HideTooltip ->
            ( { model | tooltip = Nothing }, Cmd.none )

        GotElement purl (Ok element) ->
            case model.metadata of
                Just metadata ->
                    let
                        x =
                            element.element.x + element.element.width + 10

                        y =
                            element.element.y - 10

                        term =
                            metadata.terms
                                |> List.filter (\t -> t.id == purl)
                                |> List.head
                    in
                    case term of
                        Just t ->
                            let
-- removed 8/12/19 - Kai requested to only show definition
--                                annos =
--                                    t.annotations
--                                        |> List.filter (\a -> not (List.member a.id annotationsToHide))
--                                        |> List.append
--                                            (if t.definition /= "" then
--                                                [ (Annotation "" "definition" t.definition) ]
--                                            else
--                                                []
--                                            )
                                annos =
                                    if t.definition /= "" then
                                        [ (Annotation "" "definition" t.definition) ]
                                    else
                                        []
                            in
                            ( { model | tooltip = Just (ToolTip x y annos) }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        GotElement _ (Err error) ->
--            let
--                _ = Debug.log "GotElement" (toString error)
--            in
            ( { model | tooltip = Nothing }, Cmd.none )

        ToggleUnannotatedMetadata ->
            ( { model | showUnannotatedMetadata = not model.showUnannotatedMetadata }, Cmd.none )

        CartMsg subMsg ->
            let
                newCart =
                    Cart.update subMsg (Session.getCart model.session)

                newSession =
                    Session.setCart model.session newCart
            in
            ( { model | session = newSession }
            , Cart.store newCart
            )



-- VIEW --


view : Model -> Html Msg
view model =
    case model.sample of
        Success sample ->
            let
                numExperiments =
                    model.experiments |> Maybe.map List.length |> Maybe.withDefault 0
            in
            div [ class "container" ]
                [ div [ class "pb-2 mt-5 mb-2 border-bottom", style "width" "100%" ]
                    [ h1 [ class "font-weight-bold d-inline" ]
                        [ span [ style "color" "dimgray" ] [ text "Sample" ]
                        , small [ class "ml-3", style "color" "gray" ] [ text sample.accn ]
                        ]
                    , span [ class "float-right" ]
                        [ Cart.addToCartButton (Session.getCart model.session) (Just ("Add to Cart", "Remove from Cart")) sample.files |> Html.map CartMsg ]
                    ]
                , div []
                    [ viewSample sample (model.samplingEvents |> Maybe.withDefault []) ]
                , div [ class "pt-3" ]
                    [ Page.viewTitle2 "Experiments" False
                    , span [ class "badge badge-pill badge-primary align-middle ml-2" ]
                        [ if numExperiments == 0 then
                            text ""
                          else
                            text (String.fromInt numExperiments)
                        ]
                    ]
                , div [ class "pt-2", style "overflow-y" "auto", style "max-height" "80vh" ]
                    [ viewExperiments model.experiments ]
                , div [ class "pt-3 pb-2" ]
                    [ Page.viewTitle2 "Metadata" False ]
                , viewMetadata model.metadata model.showUnannotatedMetadata
                , case model.tooltip of
                    Just tooltip ->
                        viewTooltip tooltip

                    Nothing ->
                        text ""
                ]

        Loading ->
            viewSpinner

        Failure error ->
            Error.view error False

        _ ->
            text ""


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
                                    a [ Route.href (Route.SamplingEvent event.id), class "text-nowrap" ]
                                        [ (String.Extra.toSentenceCase event.type_) ++ " " ++ event.name |> text ]
                                )
                            |> List.intersperse (text ", ")
                        )
                ]
    in
    table []
        [ tr []
            [ td [ style "min-width" "50vw" ]
                [ table [ class "table table-borderless table-sm" ]
                    [ tbody []
                        [ tr []
                            [ th [ class "w-25" ] [ text "Accession", span [ class "align-baseline ml-2"] [ Icon.externalLink ] ]
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


viewExperiments : Maybe (List Experiment) -> Html Msg
viewExperiments maybeExperiments =
    let
        mkRow exp =
            tr []
                [ td [ class "text-nowrap" ]
                    [ a [ Route.href (Route.Experiment exp.id) ] [ text exp.accn ] ]
                , td [] [ text (String.Extra.toSentenceCase exp.name) ]
                ]
    in
    case maybeExperiments |> Maybe.withDefault [] of
        [] ->
            text "None"

        experiments ->
            table [ class "table" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Accession" ]
                        , th [] [ text "Name" ]
                        ]
                    ]
                , tbody []
                    (experiments |> List.sortBy .name |> List.map mkRow)
                ]


viewMetadata : Maybe Metadata -> Bool -> Html Msg
viewMetadata maybeMetadata showUnannotated =
    case maybeMetadata of
        Just metadata ->
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
                        a [ id term.id, href purl, target "_blank", onMouseEnter (ShowTooltip term.id), onMouseLeave HideTooltip ]
                            [ text term.label ]
                    else
                        text ""

                mkUnit term =
                    if term.unitId /= "" then
                        a [ href term.unitId, title term.unitId, target "_blank" ]
                            [ text term.unitLabel ]
                    else
                        text ""

                mkSourceUrl url =
                    if url == "" then
                        text ""
                    else
                        a [ href url, target "_blank" ] [ text "Link" ]

                mkRow (term, maybeValue) =
                    tr []
                        [ td [] [ mkRdf term ]
                        , td [] [ text term.alias_ ]
                        , td [] [ maybeValue |> valueToString |> Search.viewValue ]
                        , td [] [ mkUnit term ]
                        , td [] [ mkSourceUrl term.sourceUrl ]
                        ]

                extLinkIcon =
                    span [ class "align-baseline ml-2" ] [ Icon.externalLink ]

                sortTerm a b =
                    let
                        termA =
                            Tuple.first a

                        termB =
                            Tuple.first b

                        label term =
                            if term.label == "" then
                                "~" ++ term.alias_ -- hack to sort alias after label
                            else
                                term.label
                    in
                    compare (label termA |> String.toLower) (label termB |> String.toLower)
            in
            div []
                [ table [ class "table table-sm" ]
                    [ thead []
                        [ tr []
                            [ th [ class "text-nowrap" ] [ text "Ontology Label", extLinkIcon ]
                            , th [ class "text-nowrap" ] [ text "Dataset Label" ]
                            , th [] [ text "Value" ]
                            , th [ class "text-nowrap" ] [ text "Unit", extLinkIcon ]
                            , th [ class "text-nowrap" ] [ text "Source", extLinkIcon ]
                            ]
                        ]
                    , tbody []
                        (List.Extra.zip metadata.terms metadata.values
                            |> List.filter (\t -> showUnannotated || (Tuple.first t |> .label) /= "")
                            |> List.sortWith sortTerm
                            |> List.map mkRow
                        )
                    ]
                , button [ class "btn btn-primary", onClick ToggleUnannotatedMetadata ]
                    [ if showUnannotated then
                        text "Hide Unannotated Fields"
                      else
                        text "Show Unannotated Fields"
                    ]
                ]

        Nothing ->
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
                    [ th [ class "align-top pr-3" ] [ text (String.Extra.toSentenceCase anno.label) ]
                    , td [ class "align-top" ] [ text anno.value ]
                    ]
        in
        div [ class "rounded border py-2 px-3", style "background-color" "#efefef", style "z-index" "1000", style "position" "absolute", style "top" top, style "left" left, style "max-width" "50vw" ]
            [ table []
                (List.map row tooltip.content)
            ]
    else
        text ""
