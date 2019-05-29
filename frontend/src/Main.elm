import Browser
import Browser.Navigation
import Url exposing (Url)
import Html exposing (..)
import Json.Encode as Encode exposing (Value)
import Debug exposing (toString)
import Page exposing (Page, view)
import Page.NotFound as NotFound
import Page.Blank as Blank
import Page.Home as Home
import Page.Search as Search
import Session exposing (Session)
import Route exposing (Route)



main =
--    Browser.element
--        { init = init
--        , update = update
--        , view = view
--        , subscriptions = subscriptions
--        }
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Redirect _ ->
            Sub.none

        NotFound _ ->
            Sub.none

        Home home ->
            Sub.none --Sub.map HomeMsg (Home.subscriptions home)

        Search search ->
            Sub.map SearchMsg (Search.subscriptions search)



-- MODEL


type Model
    = Redirect Session
    | NotFound Session
    | Home Home.Model
    | Search Search.Model


init : Value -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init navKey url flags =
--    let
--        ( subModel, subMsg ) =
----            Search.init flags
--            Home.init
--    in
--    ( Search subModel, Cmd.map SearchMsg subMsg )
--    ( Home subModel, Cmd.none )
    changeRouteTo (Route.fromUrl url)
        (Redirect Session.empty) --(Session.fromViewer navKey maybeViewer))



-- UPDATE


type Msg
    = HomeMsg Home.Msg
    | SearchMsg Search.Msg
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest


toSession : Model -> Session
toSession page =
    case page of
        Redirect session ->
            session

        NotFound session ->
            session

        Home home ->
            Home.toSession home

        Search search ->
            Search.toSession search


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Home ->
            Home.init session
                |> updateWith Home HomeMsg model

        Just Route.Search ->
            Search.init session
                |> updateWith Search SearchMsg model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            -- If we got a link that didn't include a fragment,
                            -- it's from one of those (href "") attributes that
                            -- we have to include to make the RealWorld CSS work.
                            --
                            -- In an application doing path routing instead of
                            -- fragment-based routing, this entire
                            -- `case url.fragment of` expression this comment
                            -- is inside would be unnecessary.
                            ( model, Cmd.none )

                        Just _ ->
                            ( model
                            , Browser.Navigation.load (Url.toString url) --Browser.Navigation.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                            )

                Browser.External href ->
                    ( model
                    , Browser.Navigation.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

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


view : Model -> Browser.Document Msg
view model =
    let
--        viewer =
--            Session.viewer (toSession model)

        viewPage page toMsg content  =
            let
                { title, body } =
                    Page.view page content
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Redirect _ ->
            Page.view Page.Other Blank.view

        NotFound _ ->
            Page.view Page.Other NotFound.view

        Home subModel ->
            viewPage Page.Home HomeMsg (Home.view subModel)

        Search subModel ->
--            Search.view subModel |> Html.map SearchMsg
            Page.view Page.Search (Search.view subModel |> Html.map SearchMsg)
