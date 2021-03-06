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
import RemoteData exposing (RemoteData(..))
import String.Extra
import Icon
import Config exposing (dataCommonsUrl, sraUrl)



---- MODEL ----


type alias Model =
    { session : Session
    , experiment : RemoteData Http.Error Experiment
    , runs : RemoteData Http.Error (List Run)
    }


init : Session -> Int -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , experiment = Loading
      , runs = Loading
      }
      , Cmd.batch
        [ Experiment.fetch id |> Http.send GetExperimentCompleted
        , Run.fetchAllByExperiment id |> Http.send GetRunsCompleted
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
        GetExperimentCompleted result ->
            ( { model | experiment = RemoteData.fromResult result }, Cmd.none )

        GetRunsCompleted result ->
            ( { model | runs = RemoteData.fromResult result }, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    Page.viewRemoteData model.experiment
        (\experiment ->
            let
                pageTitle =
                    "Experiment"

                numRuns =
                    model.runs |> RemoteData.toMaybe |> Maybe.map List.length |> Maybe.withDefault 0
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
        )


viewExperiment : Experiment -> Html Msg
viewExperiment experiment =
    table []
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


viewRuns : RemoteData Http.Error (List Run) -> Html Msg
viewRuns runs =
    let
        myLocale =
            { usLocale | decimals = 0 }

        extLinkIcon =
            span [ class "align-baseline ml-2" ] [ Icon.externalLink ]

        mkRow run =
            tr []
                [ td [ class "text-nowrap" ]
                    [ a [ href (sraUrl ++ run.accn), target "_blank" ] [ text run.accn ] ]
                , td [] [ run.totalSpots |> toFloat |> format myLocale |> text ]
                , td [] [ run.totalBases |> toFloat |> format myLocale |> text ]
                , td []
                    [ if run.files /= [] then
                        fileTable run.files
                     else
                        text "Not Available"
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
                    (files |> List.sortBy .url |> List.map viewFile)
                ]

        viewFile f =
            tr []
                [ td [] [ text (String.Extra.toSentenceCase f.type_) ]
                , td [] [ text (String.Extra.toSentenceCase f.format) ]
                , td [] [ a [ href (dataCommonsUrl ++ f.url), target "_blank" ] [ text f.url ] ]
                ]
    in
    Page.viewRemoteData runs
        (\r ->
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
                    (r |> List.sortBy .accn |> List.map mkRow)
                ]
        )