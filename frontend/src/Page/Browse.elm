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
import Debug exposing (toString)
import Project exposing (Project)



---- MODEL ----


type alias Model =
    { session : Session
    , projects : List Project
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , projects = []
      }
      , Project.fetchAll |> Http.toTask |> Task.attempt GetProjectsCompleted
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE --


type Msg
    = GetProjectsCompleted (Result Http.Error (List Project))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetProjectsCompleted (Ok projects) ->
            ( { model | projects = projects }, Cmd.none )

        GetProjectsCompleted (Err error) -> --TODO
            let
                _ = Debug.log "GetProjectsCompleted" (toString error)
            in
            ( model, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    let
        mkRow project =
            tr []
                [ td [ style "white-space" "nowrap" ]
                    [ a [ Route.href (Route.Project project.id) ] [ text project.name ] ]
                , td [] [ text project.description ]
                , td [] [ text (String.Extra.toSentenceCase project.type_) ]
                , td [] [ text (toString project.sampleCount) ]
                ]

        sortByName a b =
            compare a.name b.name

        content =
            if model.projects == [] then
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
                        (model.projects |> List.sortWith sortByName |> List.map mkRow)
                    ]
    in
    div [ class "container" ]
        [ Page.viewTitleWithoutBorder "Projects"
        , div []
            [ content
            ]
        ]