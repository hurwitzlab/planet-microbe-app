module Page.Analyze exposing (Model, Msg, init, toSession, update, view)

import Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Route exposing (Route)
import Http
import Json.Encode as Encode
--import Page.Error as Error exposing (PageLoadError)
import Task exposing (Task)



---- MODEL ----


type alias Model =
    { session : Session
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }, Cmd.none )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE --


type Msg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    div [] [ text "Analyze" ]