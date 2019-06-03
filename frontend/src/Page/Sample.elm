module Page.Sample exposing (Model, Msg, init, toSession, update, view)

import Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page
import Route
import Sample exposing (Sample)
import Http
--import Page.Error as Error exposing (PageLoadError)
import Task exposing (Task)
import String.Extra
import Debug exposing (toString)



---- MODEL ----


type alias Model =
    { session : Session
    , sample : Maybe Sample
    }


init : Session -> Int -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , sample = Nothing
      }
      , Sample.fetch id |> Http.toTask |> Task.attempt GetSampleCompleted
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE --


type Msg
    = GetSampleCompleted (Result Http.Error Sample)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetSampleCompleted (Ok sample) ->
            ( { model | sample = Just sample }, Cmd.none )

        GetSampleCompleted (Err error) -> --TODO
            let
                _ = Debug.log "GetSampleCompleted" (toString error)
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
                ]


viewSample : Sample -> Html Msg
viewSample sample =
    table [ class "table table-borderless table-sm" ]
        [ tbody []
            [ tr []
                [ th [] [ text "Accession" ]
                , td [] [  a [ href ("https://www.ncbi.nlm.nih.gov/biosample/?term=" ++ sample.accn), target "_blank" ] [ text sample.accn ] ]
                ]
            , tr []
                [ th [] [ text "Project" ]
                , td [] [ a [ Route.href (Route.Project sample.projectId) ] [ text sample.projectName ] ]
                ]
            , tr []
                [ th [] [ text (String.Extra.toSentenceCase sample.campaignType) ]
                , td [] [ a [ Route.href (Route.Campaign sample.campaignId) ] [ text sample.campaignName ] ]
                ]
            , tr []
                [ th [] [ text (String.Extra.toSentenceCase sample.samplingEventType) ]
                , td [] [ a [ Route.href (Route.SamplingEvent sample.samplingEventId) ] [ text sample.samplingEventName ] ]
                ]
            ]
        ]

