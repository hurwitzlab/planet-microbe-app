module Main exposing (main) -- this line is required for Parcel elm-hot HMR file change detection

-- From elm-spa-example:
-- Based on discussions around how asset management features
-- like code splitting and lazy loading have been shaping up, it's possible
-- that most of this file may become unnecessary in a future release of Elm.
-- Avoid putting things in this module unless there is no alternative!
-- See https://discourse.elm-lang.org/t/elm-spa-in-0-19/1800/2 for more.

import Browser
import Browser.Navigation
import Url exposing (Url)
import Http
import Task
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)
import OAuth.AuthorizationCode
import Agave
import Html exposing (..)
import Page exposing (Page, view)
import Page.Blank as Blank
import Page.Home as Home
import Page.Browse as Browse
import Page.Search as Search
import Page.Analyze as Analyze
import Page.App as App
import Page.Job as Job
import Page.Project as Project
import Page.Sample as Sample
import Page.Campaign as Campaign
import Page.SamplingEvent as SamplingEvent
import Page.Experiment as Experiment
import Page.Contact as Contact
import Page.Account as Account
import Page.Cart as Cart
import GAnalytics
import Session exposing (Session)
import Credentials exposing (Credentials)
import State exposing (State)
import Cart as CartData
import User exposing (User)
import Route exposing (Route)
import Config
--import Debug exposing (toString)



main =
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
            Sub.map GotCredentials (Credentials.onCredentialsChange (Decode.decodeString Credentials.decoder >> Result.withDefault Credentials.default))

        Job job ->
            Sub.map JobMsg (Job.subscriptions job)

        Search search ->
            Sub.map SearchMsg (Search.subscriptions search)

        Sample sample ->
            Sub.map SampleMsg (Sample.subscriptions sample)

        SamplingEvent samplingEvent ->
            Sub.map SamplingEventMsg (SamplingEvent.subscriptions samplingEvent)

        _ ->
            Sub.none



-- MODEL


type Model -- FIXME inherited this from elm-spa-example but I don't like it because of redundancy in toSession/changeRouteTo/view functions
    = Redirect Session
    | Home Session
    | Browse Browse.Model
    | Search Search.Model
    | Analyze Analyze.Model
    | App App.Model
    | Job Job.Model
    | Project Project.Model
    | Sample Sample.Model
    | Campaign Campaign.Model
    | SamplingEvent SamplingEvent.Model
    | Experiment Experiment.Model
    | Contact Contact.Model
    | Account Account.Model
    | Cart Cart.Model


type alias Flags =
    { cart : Maybe CartData.Cart
    , cred : Maybe Credentials
--    , randomCode : String
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.succeed Flags
        |> required "cart" (Decode.nullable CartData.decoder)
        |> required "cred" (Decode.nullable Credentials.decoder)
--        |> required "randomCode" Decode.string


init : Value -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        session =
            case flags |> Decode.decodeValue flagsDecoder of
                Ok f ->
                    let
                        cart =
                            f.cart |> Maybe.withDefault CartData.empty
                    in
                    case f.cred of
                        Just cred ->
                            Session.LoggedIn navKey State.default cart cred

                        Nothing ->
                            Session.Guest navKey State.default cart

                Err error ->
                    Session.Guest navKey State.default CartData.empty
    in
    case OAuth.AuthorizationCode.parseCode url of
        OAuth.AuthorizationCode.Success { code, state } ->
-- FIXME
--            let
--                _ = Debug.log "OAuth.AuthorizationCode.Success" ("code=" ++ code ++ ", " ++ "state=" ++ (toString state))
--            in
--            if Maybe.withDefault "" state /= (Session.getState session |> State.toString) ++ "." ++ randomCode then
--                let
--                    _ = Debug.log "Error: auth random code mismatch" (toString randomCode)
--                in
--                changeRouteTo (Just Route.Home) (Home session)
--            else
                let
                    stateUrl =
                        { redirectUrl | fragment = Just (state |> Maybe.withDefault "" |> String.append "/") }

                    newSession =
                        Session.setState session (State (Url.toString stateUrl))
                in
                ( Redirect newSession --model
                , Cmd.batch
                    [ getAccessToken "agave" code |> Http.toTask |> Task.attempt GotAccessToken
                    ]
                )

        OAuth.AuthorizationCode.Empty ->
            changeRouteTo (Route.fromUrl url)
                (Redirect session)

        OAuth.AuthorizationCode.Error err -> -- TODO
--            let
--                _ = Debug.log "OAuth.AuthorizationCode.Error" ""
--            in
            changeRouteTo (Route.fromUrl url)
                (Redirect session)


getAccessToken : String -> String -> Http.Request Agave.TokenResponse
getAccessToken provider code =
    let
        url =
            Config.apiBaseUrl ++ "/token"

        body =
            Encode.object
                [ ( "provider", Encode.string provider )
                , ( "code", Encode.string code )
                ]
    in
    Http.request
        { method = "POST"
        , body = Http.jsonBody body
        , headers = []
        , url = url
        , expect = Http.expectJson Agave.tokenResponseDecoder
        , timeout = Nothing
--        , tracker = Nothing
        , withCredentials = False
        }


redirectUrl =
    Config.agaveRedirectUrl |> Url.fromString |> Maybe.withDefault defaultHttpsUrl


defaultHttpsUrl =
    { protocol = Url.Https
    , host = ""
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }



-- UPDATE


type Msg
    = BrowseMsg Browse.Msg
    | SearchMsg Search.Msg
    | AnalyzeMsg Analyze.Msg
    | AppMsg App.Msg
    | JobMsg Job.Msg
    | ProjectMsg Project.Msg
    | SampleMsg Sample.Msg
    | CampaignMsg Campaign.Msg
    | SamplingEventMsg SamplingEvent.Msg
    | ExperimentMsg Experiment.Msg
    | ContactMsg Contact.Msg
    | AccountMsg Account.Msg
    | CartMsg Cart.Msg
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotAccessToken (Result Http.Error Agave.TokenResponse)
    | GotUserInfo (Result Http.Error User)
    | GotCredentials Credentials


toSession : Model -> Session
toSession page =
    case page of
        Redirect session ->
            session

        Home session ->
            session

        Browse browse ->
            Browse.toSession browse

        Search search ->
            Search.toSession search

        Analyze analyze ->
            Analyze.toSession analyze

        App app ->
            App.toSession app

        Job job ->
            Job.toSession job

        Project project ->
            Project.toSession project

        Sample sample ->
            Sample.toSession sample

        Campaign campaign ->
            Campaign.toSession campaign

        SamplingEvent samplingEvent ->
            SamplingEvent.toSession samplingEvent

        Experiment experiment ->
            Experiment.toSession experiment

        Contact contact ->
            Contact.toSession contact

        Account account ->
            Account.toSession account

        Cart cart ->
            Cart.toSession cart


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( Home session, Cmd.none )

        Just route ->
            let
                state =
                    Session.getState session

                newState =
                    { state | url = Route.routeToString route |> String.dropLeft 2 } -- remove leading "#/"

                newSession =
                    Session.setState session newState

                ( newModel, newCmd ) =
                    case route of
                        Route.Login ->
                            let
                                authorizationUrl =
                                    Config.agaveBaseUrl ++ "/authorize" |> Url.fromString |> Maybe.withDefault defaultHttpsUrl

                                auth =
                                    { clientId = Config.agaveOAuthClientId
                                    , redirectUri = redirectUrl
                                    , scope = [ "PRODUCTION" ]
                                    , state = Just (Session.getState session |> State.toString)
                                    , url = authorizationUrl
                                    }
                            in
                            ( model
                            , auth |> OAuth.AuthorizationCode.makeAuthUrl |> Url.toString |> Browser.Navigation.load -- redirect to CyVerse login page
                            )

                        Route.Logout ->
                            let
                                ( newModel2, newCmd2 ) =
                                    changeRouteTo (Just Route.Home) (Home (Session.logout session))
                            in
                            ( newModel2
                            , Cmd.batch
                                [ Credentials.storeCredentials Nothing
                                , Route.replaceUrl (Session.navKey session) (Route.Home)
                                , newCmd2
                                ]
                            )

                        Route.Home ->
                            ( Home newSession, Cmd.none )

                        Route.Browse ->
                            Browse.init newSession
                                |> updateWith Browse BrowseMsg model

                        Route.Search ->
                            Search.init newSession
                                |> updateWith Search SearchMsg model

                        Route.Analyze tab ->
                            Analyze.init newSession tab
                                |> updateWith Analyze AnalyzeMsg model

                        Route.App id ->
                            App.init newSession id
                                |> updateWith App AppMsg model

                        Route.Job id ->
                            Job.init newSession id
                                |> updateWith Job JobMsg model

                        Route.Project id ->
                            Project.init newSession id
                                |> updateWith Project ProjectMsg model

                        Route.Sample id ->
                            Sample.init newSession id
                                |> updateWith Sample SampleMsg model

                        Route.Campaign id ->
                            Campaign.init newSession id
                                |> updateWith Campaign CampaignMsg model

                        Route.SamplingEvent id ->
                            SamplingEvent.init newSession id
                                |> updateWith SamplingEvent SamplingEventMsg model

                        Route.Experiment id ->
                            Experiment.init newSession id
                                |> updateWith Experiment ExperimentMsg model

                        Route.Contact ->
                            Contact.init newSession
                                |> updateWith Contact ContactMsg model

                        Route.Account ->
                            Account.init newSession
                                |> updateWith Account AccountMsg model

                        Route.Cart ->
                            Cart.init newSession
                                |> updateWith Cart CartMsg model
            in
            ( newModel
            , Cmd.batch
                [ newCmd
                , GAnalytics.send (Route.routeToString route) Config.googleAnalyticsTrackingId -- Google Analytics
                ]
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        session =
            toSession model
    in
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
                            , Browser.Navigation.pushUrl (Session.navKey session) (Url.toString url)
                            )

                Browser.External href ->
                    ( model
                    , Browser.Navigation.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( BrowseMsg subMsg, Browse subModel ) ->
            Browse.update subMsg subModel
                |> updateWith Browse BrowseMsg model

        ( SearchMsg subMsg, Search subModel ) ->
            Search.update subMsg subModel
                |> updateWith Search SearchMsg model

        ( AnalyzeMsg subMsg, Analyze subModel ) ->
            Analyze.update subMsg subModel
                |> updateWith Analyze AnalyzeMsg model

        ( AppMsg subMsg, App subModel ) ->
            App.update subMsg subModel
                |> updateWith App AppMsg model

        ( JobMsg subMsg, Job subModel ) ->
            Job.update subMsg subModel
                |> updateWith Job JobMsg model

        ( ProjectMsg subMsg, Project subModel ) ->
            Project.update subMsg subModel
                |> updateWith Project ProjectMsg model

        ( SampleMsg subMsg, Sample subModel ) ->
            Sample.update subMsg subModel
                |> updateWith Sample SampleMsg model

        ( CampaignMsg subMsg, Campaign subModel ) ->
            Campaign.update subMsg subModel
                |> updateWith Campaign CampaignMsg model

        ( SamplingEventMsg subMsg, SamplingEvent subModel ) ->
            SamplingEvent.update subMsg subModel
                |> updateWith SamplingEvent SamplingEventMsg model

        ( ExperimentMsg subMsg, Experiment subModel ) ->
            Experiment.update subMsg subModel
                |> updateWith Experiment ExperimentMsg model

        ( ContactMsg subMsg, Contact subModel ) ->
            Contact.update subMsg subModel
                |> updateWith Contact ContactMsg model

        ( AccountMsg subMsg, Account subModel ) ->
            Account.update subMsg subModel
                |> updateWith Account AccountMsg model

        ( CartMsg subMsg, Cart subModel ) ->
            Cart.update subMsg subModel
                |> updateWith Cart CartMsg model

        ( GotAccessToken res, _ ) ->
            case res of
--                Err (Http.BadBody body) ->
--                    case Json.decodeString OAuth.AuthorizationCode.defaultAuthenticationErrorDecoder body of
--                        Ok { error, errorDescription } ->
--                            let
--                                errMsg =
--                                    "Unable to retrieve token: " ++ errorResponseToString { error = error, errorDescription = errorDescription }
--                            in
--                            ( { model | error = Just errMsg }
--                            , Cmd.nonex
--                            )
--
--                        _ ->
--                            ( { model | error = Just ("Unable to retrieve token: " ++ body) }
--                            , Cmd.none
--                            )

                Err _ -> --FIXME
--                    ( Login { subModel | error = Just "Unable to retrieve token: HTTP request failed. CORS is likely disabled on the authorization server." }
--                    , Cmd.none
--                    )
                    ( model, Cmd.none )

                Ok { accessToken, refreshToken, expiresIn } ->
                    let
                        defaultCred =
                            Credentials.default

                        newCred =
                            { defaultCred | token = accessToken, refreshToken = refreshToken, expiresIn = Just expiresIn, user = Nothing }

                        newSession =
                            Session.setCredentials session newCred
                    in
                    ( Redirect newSession
                    , Cmd.batch
                          [ Credentials.store newCred
                          , User.recordLogin accessToken |> Http.toTask |> Task.attempt GotUserInfo
                          ]
                    )

        ( GotUserInfo res, _ ) ->
            case res of
                Err _ -> --FIXME
--                    ( Login { subModel | error = Just "Unable to retrieve user profile: HTTP request failed." }
--                    , Cmd.none
--                    )
                    ( model, Cmd.none )

                Ok user ->
                    let
                        cred =
                            Session.credentials session |> Maybe.withDefault Credentials.default

                        newCred =
                            { cred | user = Just user }

                        newSession =
                            Session.setCredentials session newCred

                        stateUrl =
                            Session.getState session |> .url |> Url.fromString |> Maybe.withDefault defaultHttpsUrl

                        (newModel, newCmd) =
                            changeRouteTo (Route.fromUrl stateUrl)
                                (Redirect newSession)
                    in
                    ( newModel
                    , Cmd.batch
                        [ Credentials.store newCred
                        , Browser.Navigation.replaceUrl (Session.navKey session) (Url.toString stateUrl)
                        , newCmd
                        ]
                    )

        ( GotCredentials newCredentials, _ ) -> --Redirect _ ) -> -- Cookie was updated in another window/tab
            let
                newSession =
                    Session.setCredentials session newCredentials
            in
            ( Redirect newSession --FIXME
            , Cmd.none
            )

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
        session =
            toSession model
    in
    case model of
        Redirect _ ->
            Page.view session Page.Other Blank.view

        Home _ ->
            Page.view session Page.Home Home.view

        Browse subModel ->
            Page.view session Page.Browse (Browse.view subModel |> Html.map BrowseMsg)

        Search subModel ->
            Page.view session Page.Search (Search.view subModel |> Html.map SearchMsg)

        Analyze subModel ->
            Page.view session Page.Analyze (Analyze.view subModel |> Html.map AnalyzeMsg)

        App subModel ->
            Page.view session Page.App (App.view subModel |> Html.map AppMsg)

        Job subModel ->
            Page.view session Page.Job (Job.view subModel |> Html.map JobMsg)

        Project subModel ->
            Page.view session Page.Project (Project.view subModel |> Html.map ProjectMsg)

        Sample subModel ->
            Page.view session Page.Sample (Sample.view subModel |> Html.map SampleMsg)

        Campaign subModel ->
            Page.view session Page.Campaign (Campaign.view subModel |> Html.map CampaignMsg)

        SamplingEvent subModel ->
            Page.view session Page.SamplingEvent (SamplingEvent.view subModel |> Html.map SamplingEventMsg)

        Experiment subModel ->
            Page.view session Page.Experiment (Experiment.view subModel |> Html.map ExperimentMsg)

        Contact subModel ->
            Page.view session Page.Contact (Contact.view subModel |> Html.map ContactMsg)

        Account subModel ->
            Page.view session Page.Account (Account.view subModel |> Html.map AccountMsg)

        Cart subModel ->
            Page.view session Page.Cart (Cart.view subModel |> Html.map CartMsg)