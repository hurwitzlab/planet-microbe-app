module Page.Project exposing (Model, Msg, init, toSession, update, view)

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
    , project : Maybe Project
    }


init : Session -> Int -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , project = Nothing
      }
      , getProject |> Http.toTask |> Task.attempt GetProjectCompleted
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE --


type Msg
    = GetProjectCompleted (Result Http.Error Project)


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


getProject : Http.Request Project
getProject =
    let
        url =
            apiBaseUrl ++ "/projects"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decodeProject)
        |> HttpBuilder.toRequest


type alias Project  =
    { id : Int
    , name : String
    , accn : String
    , description : String
    , sampleCount : Int
    }


decodeProject : Decoder Project
decodeProject =
    Decode.succeed Project
        |> required "project_id" Decode.int
        |> required "name" Decode.string
        |> required "accn" Decode.string
        |> required "description" Decode.string
        |> required "sample_count" Decode.int



-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ Page.viewTitleWithoutBorder "Project"
        , div [ class "row" ]
            []
        ]