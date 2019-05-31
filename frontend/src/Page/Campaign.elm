module Page.Campaign exposing (Model, Msg, init, toSession, update, view)

import Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page
import Route
import Campaign exposing (Campaign)
import Http
--import Page.Error as Error exposing (PageLoadError)
import Task exposing (Task)
import String.Extra
import Debug exposing (toString)



---- MODEL ----


type alias Model =
    { session : Session
    , campaign : Maybe Campaign
    }


init : Session -> Int -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , campaign = Nothing
      }
      , Campaign.fetch id |> Http.toTask |> Task.attempt GetCampaignCompleted
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE --


type Msg
    = GetCampaignCompleted (Result Http.Error Campaign)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetCampaignCompleted (Ok campaign) ->
            ( { model | campaign = Just campaign }, Cmd.none )

        GetCampaignCompleted (Err error) -> --TODO
            let
                _ = Debug.log "GetCampaignCompleted" (toString error)
            in
            ( model, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    case model.campaign of
        Nothing ->
            text ""

        Just campaign ->
            div [ class "container" ]
                [ Page.viewTitle (String.Extra.toSentenceCase campaign.type_) campaign.name
                , div []
                    [ viewCampaign campaign ]
                ]


viewCampaign : Campaign -> Html Msg
viewCampaign campaign =
    table [ class "table table-borderless table-sm" ]
        [ tbody []
            [ tr []
                [ th [] [ text "Name" ]
                , td [] [ text campaign.name ]
                ]
            , tr []
                [ th [] [ text "Description" ]
                , td [] [ text campaign.description ]
                ]
            ]
        ]
