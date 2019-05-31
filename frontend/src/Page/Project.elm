module Page.Project exposing (Model, Msg, init, toSession, update, view)

import Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page
import Project exposing (Project)
import Sample exposing (Sample)
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
    , samples : Maybe (List Sample)
    }


init : Session -> Int -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , project = Nothing
      , samples = Nothing
      }
      , Cmd.batch
        [ Project.fetch id |> Http.toTask |> Task.attempt GetProjectCompleted
        , Sample.fetchAllByProject id |> Http.toTask |> Task.attempt GetSamplesCompleted
        ]
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE --


type Msg
    = GetProjectCompleted (Result Http.Error Project)
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
                numSamples =
                    model.samples |> Maybe.map List.length |> Maybe.withDefault 0
            in
            div [ class "container" ]
                [ div []
                    [ Page.viewTitle "Project" project.name ]
                , div []
                    [ viewProject project ]
                , div [ class "pt-3" ]
                    [ Page.viewTitle2 "Samples" False
                    , span [ class "badge badge-pill badge-primary align-middle ml-2" ]
                        [ if numSamples == 0 then
                            text ""
                          else
                            text (toString numSamples)
                        ]
                    ]
                , div [ class "pt-2" ]
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
    case maybeSamples of
        Nothing ->
            text "None"

        Just samples ->
            table [ class "table" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Accn" ]
                        ]
                    ]
                , tbody []
                    (samples |> List.sortWith sortByAccn |> List.map mkRow)
                ]