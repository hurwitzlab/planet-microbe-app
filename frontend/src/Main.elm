import Browser exposing (Document)
import Html exposing (..)
import Json.Encode as Encode exposing (Value, string)
import Time exposing (Weekday(..))
import Time
import GMap
import Debug exposing (toString)
import Config exposing (apiBaseUrl)
import Page.Search as Search
import Session exposing (Session)



main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotFound _ ->
            Sub.none

        Home home ->
            Sub.none --Sub.map GotHomeMsg (Home.subscriptions home)

        Search search ->
            Sub.map SearchMsg (Search.subscriptions search)



-- MODEL


type Model
    = NotFound Session
    | Home Session
    | Search Search.Model


init : Value -> ( Model, Cmd Msg )
init flags =
    let
        ( subModel, subMsg ) =
            Search.init flags
    in
    ( Search subModel, Cmd.map SearchMsg subMsg )



-- UPDATE


type Msg
    = InputTimerTick Time.Posix
    | SearchMsg Search.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( InputTimerTick time, Search subModel ) ->
            let
                _ = Debug.log "blah" "blah"
            in
            Search.update (Search.InputTimerTick time) subModel
                |> updateWith Search SearchMsg model

        ( SearchMsg subMsg, Search subModel ) ->
            Search.update subMsg subModel
                |> updateWith Search SearchMsg model

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- VIEW


view : Model -> Html Msg --Document Msg
view model =
--    let
--        viewer =
--            Session.viewer (toSession model)
--
--        viewPage page toMsg config =
--            let
--                { title, body } =
--                    Page.view viewer page config
--            in
--            { title = title
--            , body = List.map (Html.map toMsg) body
--            }
--    in
    case model of
--        NotFound _ ->
--            Page.view viewer Page.Other NotFound.view

        Search subModel ->
            Search.view subModel |> Html.map SearchMsg

        _ ->
            text ""