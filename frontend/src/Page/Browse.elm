module Page.Browse exposing (Model, Msg, init, toSession, update, view)

import Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page
import Route exposing (Route)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
--import Page.Error as Error exposing (PageLoadError)
import Task exposing (Task)
import String.Extra
--import Debug exposing (toString)
import Project exposing (Project)
import Sample exposing (Sample)



---- MODEL ----


type alias Model =
    { session : Session
    , projects : List Project
    , samples : List Sample
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , projects = []
      , samples = []
      }
      , Cmd.batch
        [ Project.fetchAll |> Http.toTask |> Task.attempt GetProjectsCompleted
        , Sample.fetchAll |> Http.toTask |> Task.attempt GetSamplesCompleted
        ]
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE --


type Msg
    = GetProjectsCompleted (Result Http.Error (List Project))
    | GetSamplesCompleted (Result Http.Error (List Sample))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetProjectsCompleted (Ok projects) ->
            ( { model | projects = projects }, Cmd.none )

        GetProjectsCompleted (Err error) -> --TODO
--            let
--                _ = Debug.log "GetProjectsCompleted" (toString error)
--            in
            ( model, Cmd.none )

        GetSamplesCompleted (Ok samples) ->
            ( { model | samples = samples }, Cmd.none )

        GetSamplesCompleted (Err error) -> --TODO
--            let
--                _ = Debug.log "GetSamplesCompleted" (toString error)
--            in
            ( model, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    let
        numProjects =
            List.length model.projects

        numSamples =
            List.length model.samples
    in
    div [ class "container" ]
        [ div [ class "pt-5" ]
            [ Page.viewTitle1 "Projects" False
            , span [ class "badge badge-pill badge-primary align-middle ml-2" ]
                [ if numProjects == 0 then
                    text ""
                  else
                    text (String.fromInt numProjects)
                ]
            ]
        , div [ class "pt-2" ]
            [ viewProjects model.projects
            ]
        , div [ class "pt-4" ]
            [ Page.viewTitle1 "Samples" False
            , span [ class "badge badge-pill badge-primary align-middle ml-2" ]
                [ if numSamples == 0 then
                    text ""
                  else
                    text (String.fromInt numSamples)
                ]
            ]
        , div [ class "pt-2", style "overflow-y" "auto", style "max-height" "80vh"]
            [ viewSamples model.samples
            ]
        ]


viewProjects : List Project -> Html Msg
viewProjects projects =
    let
        mkRow project =
            tr []
                [ td [ style "white-space" "nowrap" ]
                    [ a [ Route.href (Route.Project project.id) ] [ text project.name ] ]
                , td [] [ text project.description ]
                , td [] [ text (String.Extra.toSentenceCase project.type_) ]
                , td [] [ text (String.fromInt project.sampleCount) ]
                ]

        sortByName a b =
            compare a.name b.name
    in
    if projects == [] then
        text "None"
    else
        table [ class "table" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Name" ]
                    , th [] [ text "Description" ]
                    , th [] [ text "Type" ]
                    , th [] [ text "Samples" ]
                    ]
                ]
            , tbody []
                (projects |> List.sortWith sortByName |> List.map mkRow)
            ]


viewSamples : List Sample -> Html Msg
viewSamples samples =
    let
        mkRow sample =
            tr []
                [ td [ style "white-space" "nowrap" ]
                    [ a [ Route.href (Route.Sample sample.id) ] [ text sample.accn ] ]
                , td [ style "white-space" "nowrap" ]
                    [ a [ Route.href (Route.Project sample.projectId) ] [ text sample.projectName ] ]
                ]

        sortByAccn a b =
            compare a.accn b.accn
    in
    if samples == [] then
        text "None"
    else
        table [ class "table" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Accn" ]
                    , th [] [ text "Project" ]
                    ]
                ]
            , tbody []
                (samples |> List.sortWith sortByAccn |> List.map mkRow)
            ]