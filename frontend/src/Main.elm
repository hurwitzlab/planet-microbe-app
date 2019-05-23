import Browser exposing (Document)
import Html exposing (..)
import Json.Encode as Encode exposing (Value)
import Debug exposing (toString)
import Page exposing (view)
import Page.Search as Search
import Page.Home as Home
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
        NotFound ->
            Sub.none

        Home ->
            Sub.none

        Search search ->
            Sub.map SearchMsg (Search.subscriptions search)



-- MODEL


type Model
    = NotFound
    | Home
    | Search Search.Model


init : Value -> ( Model, Cmd Msg )
init flags =
    let
        ( subModel, subMsg ) =
--            Search.init flags
            Home.init
    in
--    ( Search subModel, Cmd.map SearchMsg subMsg )
    ( Home, Cmd.none )



-- UPDATE


type Msg
    = HomeMsg Home.Msg
    | SearchMsg Search.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
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

        Home ->
            Page.view Home.view |> Html.map HomeMsg

        Search subModel ->
--            Search.view subModel |> Html.map SearchMsg
            Page.view (Search.view subModel |> Html.map SearchMsg)

        _ ->
            text ""