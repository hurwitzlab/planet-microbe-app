module Page.Account exposing (Model, Msg, init, toSession, update, view)

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



---- MODEL ----


type alias Model =
    { session : Session
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      }
    , Cmd.none
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE --


type Msg
    = TODO


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TODO ->
            ( model, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    text "foo"