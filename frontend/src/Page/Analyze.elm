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
    div [ class "container" ]
        [ div [ class "jumbotron", style "margin-top" "10vh" ]
            [ div [ class "row h3" ] [ text "Coming soon ..." ]
            , div [ class "row" ]
                [ p [] [ text "In the near future you will be able to run common tools and visualize the results here.  Check back often!" ] ]
            ]
        ]