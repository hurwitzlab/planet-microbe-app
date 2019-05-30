module Page.Sample exposing (Model, Msg, init, toSession, update, view)

import Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page
import Route exposing (Route)
import Http
import HttpBuilder
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
--import Page.Error as Error exposing (PageLoadError)
import Task exposing (Task)
import Config exposing (apiBaseUrl)
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
      , getSample |> Http.toTask |> Task.attempt GetSampleCompleted
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


getSample : Http.Request Sample
getSample =
    let
        url =
            apiBaseUrl ++ "/samples"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decodeSample)
        |> HttpBuilder.toRequest


type alias Sample  =
    { name : String
    , accn : String
    , description : String
    }


decodeSample : Decoder Sample
decodeSample =
    Decode.succeed Sample
        |> required "name" Decode.string
        |> required "accn" Decode.string
        |> required "description" Decode.string



-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ Page.viewTitleWithoutBorder "Sample"
        , div [ class "row" ]
            []
        ]