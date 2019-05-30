module Page.Project exposing (Model, Msg, init, toSession, update, view)

import Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page
import Project exposing (Project)
import Http
--import Page.Error as Error exposing (PageLoadError)
import Task exposing (Task)
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
      , Project.fetch id |> Http.toTask |> Task.attempt GetProjectCompleted
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



-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ Page.viewTitleWithoutBorder "Project"
        , div [ class "row" ]
            []
        ]