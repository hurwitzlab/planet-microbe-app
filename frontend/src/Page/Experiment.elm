module Page.Experiment exposing (Model, Msg, init, toSession, update, view)

import Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Page
import Route
import Experiment exposing (Experiment)
import Run exposing (Run)
import Http
--import Page.Error as Error exposing (PageLoadError)
import Task exposing (Task)
import String.Extra
import Json.Encode as Encode
--import Debug exposing (toString)
import Config exposing (discoveryEnvironmentUrl)



---- MODEL ----


type alias Model =
    { session : Session
    , experiment : Maybe Experiment
    , runs : Maybe (List Run)
    }


init : Session -> Int -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , experiment = Nothing
      , runs = Nothing
      }
      , Cmd.batch
        [ Experiment.fetch id |> Http.toTask |> Task.attempt GetExperimentCompleted
        , Run.fetchAllByExperiment id |> Http.toTask |> Task.attempt GetRunsCompleted
        ]
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE --


type Msg
    = GetExperimentCompleted (Result Http.Error Experiment)
    | GetRunsCompleted (Result Http.Error (List Run))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetExperimentCompleted (Ok experiment) ->
            ( { model | experiment = Just experiment }, Cmd.none )

        GetExperimentCompleted (Err error) -> --TODO
--            let
--                _ = Debug.log "GetExperimentCompleted" (toString error)
--            in
            ( model, Cmd.none )

        GetRunsCompleted (Ok runs) ->
            ( { model | runs = Just runs }, Cmd.none )

        GetRunsCompleted (Err error) -> --TODO
--            let
--                _ = Debug.log "GetRunsCompleted" (toString error)
--            in
            ( model, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    case model.experiment of
        Nothing ->
            text ""

        Just experiment ->
            let
                pageTitle =
                    "Experiment"

                numRuns =
                    model.runs |> Maybe.map List.length |> Maybe.withDefault 0
            in
            div [ class "container" ]
                [ Page.viewTitle pageTitle experiment.accn
                , div []
                    [ viewExperiment experiment ]
                , div [ class "pt-3" ]
                    [ Page.viewTitle2 "Runs" False
                    , span [ class "badge badge-pill badge-primary align-middle ml-2" ]
                        [ if numRuns == 0 then
                            text ""
                          else
                            text (String.fromInt numRuns)
                        ]
                    ]
                , div [ class "pt-2" ]
                    [ viewRuns model.runs ]
                ]


viewExperiment : Experiment -> Html Msg
viewExperiment experiment =
    table [] -- ugh, use table for layout
        [ tr []
            [ td [ style "min-width" "50vw" ]
                [ table [ class "table table-borderless table-sm" ]
                    [ tbody []
                        [ tr []
                            [ th [ class "w-25" ] [ text "Name/ID" ]
                            , td [] [ text experiment.name ]
                            ]
                        , tr []
                            [ th [] [ text "Accession" ]
                            , td [] [ text experiment.accn ]
                            ]
                        , tr []
                            [ th [] [ text "Sample" ]
                            , td [] [ a [ Route.href (Route.Sample experiment.sampleId) ] [ text experiment.sampleAccn ] ]
                            ]
                        , tr []
                            [ th [] [ text "Project" ]
                            , td [] [ a [ Route.href (Route.Project experiment.projectId) ] [ text experiment.projectName ] ]
                            ]
                        , tr []
                            [ th [] [ text "Library Name" ]
                            , td [] [ text experiment.library_name ]
                            ]
                        , tr []
                            [ th [] [ text "Library Strategy" ]
                            , td [] [ text experiment.library_strategy ]
                            ]
                        , tr []
                            [ th [] [ text "Library Source" ]
                            , td [] [ text experiment.library_source ]
                            ]
                        , tr []
                            [ th [] [ text "Library Selection" ]
                            , td [] [ text experiment.library_selection ]
                            ]
                        , tr []
                            [ th [] [ text "Library Protocol" ]
                            , td [] [ text experiment.library_protocol ]
                            ]
                        , tr []
                            [ th [] [ text "Library Layout" ]
                            , td [] [ text experiment.library_layout ]
                            ]
                        , tr []
                            [ th [] [ text "Average Read Length" ]
                            , td []
                                [ case experiment.library_length of
                                    Nothing ->
                                        text "Unknown"

                                    Just length ->
                                        text (String.fromInt length)
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewRuns : Maybe (List Run) -> Html Msg
viewRuns maybeRuns =
    let
        myLocale =
            { usLocale | decimals = 0 }

        extLinkIcon =
            i [ class "fas fa-external-link-alt fa-xs align-baseline ml-2" ] []

        mkRow run =
            tr []
                [ td [ class "text-nowrap" ]
                    [ a [ href ("https://www.ncbi.nlm.nih.gov/sra/?term=" ++ run.accn), target "_blank" ] [ text run.accn ] ]
                , td [] [ run.totalSpots |> toFloat |> format myLocale |> text ]
                , td [] [ run.totalBases |> toFloat |> format myLocale |> text ]
                , td []
                    [ if run.files /= [] then
                        fileTable run.files
                     else
                        text "None"
                    ]
                ]

        fileTable files =
            table [ class "table table-borderless table-sm small" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Type" ]
                        , th [] [ text "Format" ]
                        , th [] [ text "Path" ]
                        ]
                    ]
                , tbody []
                    (files |> List.sortWith sortByUrl |> List.map viewFile)
                ]

        viewFile f =
            tr []
                [ td [] [ text (String.Extra.toSentenceCase f.type_) ]
                , td [] [ text (String.Extra.toSentenceCase f.format) ]
                , td [] [ a [ href (discoveryEnvironmentUrl ++ f.url), target "_blank" ] [ text f.url ] ]
                ]

        sortByAccn a b =
            compare a.accn b.accn

        sortByUrl a b =
            compare a.url b.url
    in
    case maybeRuns |> Maybe.withDefault [] of
        [] ->
            text "None"

        runs ->
            table [ class "table" ]
                [ thead []
                    [ tr []
                        [ th [ class "text-nowrap" ] [ text "Accession", extLinkIcon ]
                        , th [] [ text "Total Spots" ]
                        , th [] [ text "Total Bases" ]
                        , th [] [ text "Files" ]
                        ]
                    ]
                , tbody []
                    (runs |> List.sortWith sortByAccn |> List.map mkRow)
                ]