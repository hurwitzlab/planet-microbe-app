module Main exposing (..) -- this line is required for Parcel elm-hot HMR file change detection

import Browser
import Browser.Navigation
import Url exposing (Url)
import Http
import Task
import OAuth
import OAuth.AuthorizationCode
import Agave
import Html exposing (..)
import Json.Encode as Encode exposing (Value)
import Page exposing (Page, view)
import Page.NotFound as NotFound
import Page.Blank as Blank
import Page.Home as Home
import Page.Browse as Browse
import Page.Search as Search
import Page.Analyze as Analyze
import Page.App as App
import Page.Project as Project
import Page.Sample as Sample
import Page.Campaign as Campaign
import Page.SamplingEvent as SamplingEvent
import Page.Experiment as Experiment
import Page.Contact as Contact
import GAnalytics
import Session exposing (Session)
import User exposing (User)
import Route exposing (Route)
import Config
import Debug exposing (toString)



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
        Search search ->
            Sub.map SearchMsg (Search.subscriptions search)

        Sample sample ->
            Sub.map SampleMsg (Sample.subscriptions sample)

        SamplingEvent samplingEvent ->
            Sub.map SamplingEventMsg (SamplingEvent.subscriptions samplingEvent)

        _ ->
            Sub.none



-- MODEL


type Model
    = Redirect Session
    | NotFound Session
    | Login Session
    | Home Home.Model
    | Browse Browse.Model
    | Search Search.Model
    | Analyze Analyze.Model
    | App App.Model
    | Project Project.Model
    | Sample Sample.Model
    | Campaign Campaign.Model
    | SamplingEvent SamplingEvent.Model
    | Experiment Experiment.Model
    | Contact Contact.Model


init : Value -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        defaultSession =
            Session.default

        (model, msg) =
            changeRouteTo (Route.fromUrl url)
--                (Redirect (Session.fromViewer navKey maybeViewer))
                (Redirect { defaultSession | navKey = Just navKey })
    in
    case OAuth.AuthorizationCode.parseCode url of
        OAuth.AuthorizationCode.Success { code, state } ->
            let
                _ = Debug.log "code" code
            in
--            if Maybe.map randomBytesFromState state /= Just model.state then
--                ( { model | error = Just "'state' doesn't match, the request has likely been forged by an adversary!" }
--                , Cmd.none
--                )
--
--            else
                ( model
                , getAccessToken "agave" code |> Http.toTask |> Task.attempt GotAccessToken --config model.redirectUri code
                )

        OAuth.AuthorizationCode.Empty ->
            ( model, Cmd.none )

        OAuth.AuthorizationCode.Error err ->
            ( model, Cmd.none )
--            ( { model | error = Just (OAuth.errorCodeToString err.error) }
--            , Cmd.none
--            )


redirectUrl =
    { defaultHttpsUrl | protocol = Url.Http, host = "localhost", port_ = Just 1234 }


defaultHttpsUrl =
    { protocol = Url.Https
    , host = ""
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }


getAccessToken : String -> String -> Http.Request Agave.TokenResponse
getAccessToken provider code =
    let
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
        , url = Config.apiBaseUrl ++ "/token"
        , expect = Http.expectJson Agave.tokenResponseDecoder
        , timeout = Nothing
--        , tracker = Nothing
        , withCredentials = False
        }


randomBytesFromState : String -> String
randomBytesFromState str =
    str
        |> stringDropLeftUntil (\c -> c == ".")


stringDropLeftUntil : (String -> Bool) -> String -> String
stringDropLeftUntil predicate str =
    let
        ( h, q ) =
            ( String.left 1 str, String.dropLeft 1 str )
    in
    if q == "" || predicate h then
        q

    else
        stringDropLeftUntil predicate q



-- UPDATE


type Msg
    = HomeMsg Home.Msg
    | BrowseMsg Browse.Msg
    | SearchMsg Search.Msg
    | AnalyzeMsg Analyze.Msg
    | AppMsg App.Msg
    | ProjectMsg Project.Msg
    | SampleMsg Sample.Msg
    | CampaignMsg Campaign.Msg
    | SamplingEventMsg SamplingEvent.Msg
    | ExperimentMsg Experiment.Msg
    | ContactMsg Contact.Msg
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotAccessToken (Result Http.Error Agave.TokenResponse) --OAuth.AuthorizationCode.AuthenticationSuccess)
    | GotUserInfo (Result Http.Error User)--(Agave.Response Agave.Profile))
    | GotSession Session


toSession : Model -> Session
toSession page =
    case page of
        Redirect session ->
            session

        NotFound session ->
            session

        Login session ->
            session

        Home home ->
            Home.toSession home

        Browse browse ->
            Browse.toSession browse

        Search search ->
            Search.toSession search

        Analyze analyze ->
            Analyze.toSession analyze

        App app ->
            App.toSession app

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


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just route ->
            let
                ( newModel, newCmd ) =
                    case route of
                        Route.Home ->
                            Home.init session
                                |> updateWith Home HomeMsg model

                        Route.Login ->
                            let
                                auth =
                                    { clientId = Config.agaveOAuthClientId
                                    , redirectUri = redirectUrl
                                    , scope = [ "PRODUCTION" ]
                                    , state = Nothing
                                    , url = authorizationUrl
                                    }

                                authorizationUrl =
                                    Config.agaveBaseUrl ++ "/authorize" |> Url.fromString |> Maybe.withDefault defaultHttpsUrl
                            in
                            ( model
                            , auth |> OAuth.AuthorizationCode.makeAuthUrl |> Url.toString |> Browser.Navigation.load
                            )

                        Route.Logout ->
                            ( model, Cmd.none ) --TODO

                        Route.Browse ->
                            Browse.init session
                                |> updateWith Browse BrowseMsg model

                        Route.Search ->
                            Search.init session
                                |> updateWith Search SearchMsg model

                        Route.Analyze ->
                            Analyze.init session
                                |> updateWith Analyze AnalyzeMsg model

                        Route.App id ->
                            App.init session id
                                |> updateWith App AppMsg model

                        Route.Project id ->
                            Project.init session id
                                |> updateWith Project ProjectMsg model

                        Route.Sample id ->
                            Sample.init session id
                                |> updateWith Sample SampleMsg model

                        Route.Campaign id ->
                            Campaign.init session id
                                |> updateWith Campaign CampaignMsg model

                        Route.SamplingEvent id ->
                            SamplingEvent.init session id
                                |> updateWith SamplingEvent SamplingEventMsg model

                        Route.Experiment id ->
                            Experiment.init session id
                                |> updateWith Experiment ExperimentMsg model

                        Route.Contact ->
                            Contact.init session
                                |> updateWith Contact ContactMsg model
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
                            , Browser.Navigation.load (Url.toString url) --Browser.Navigation.pushUrl (Session.navKey (toSession model)) (Url.toString url)
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

                Err _ ->
--                    ( Login { subModel | error = Just "Unable to retrieve token: HTTP request failed. CORS is likely disabled on the authorization server." }
--                    , Cmd.none
--                    )
                    ( model, Cmd.none )

                Ok { accessToken, refreshToken, expiresIn } ->
                    let
                        _ = Debug.log "token" accessToken

                        newSession =
                            { session |
                                token = accessToken
                                , refreshToken = refreshToken
                                , expiresIn = Just expiresIn
                            }
                    in
--                    ( Login { subModel | token = Just token }
--                    , Agave.getProfile (OAuth.tokenToString token) |> Http.toTask |> Task.attempt GotUserInfo --getUserInfo token
--                    )
                    ( model,
                      Cmd.batch
                          [ User.recordLogin accessToken |> Http.toTask |> Task.attempt GotUserInfo --Agave.getProfile accessToken |> Http.toTask |> Task.attempt GotUserInfo
                          , Session.store newSession
                          ]
                    )

        ( GotUserInfo res, _ ) ->
            let
                _ = Debug.log "GotUserInfo" (toString res)
            in
            case res of
                Err _ ->
--                    ( Login { subModel | error = Just "Unable to retrieve user profile: HTTP request failed." }
--                    , Cmd.none
--                    )
                    ( model, Cmd.none )

                Ok user ->
                    let
                        newSession =
                            { session | user = Just user }
                    in
--                    ( Login { subModel | profile = Just response.result }
--                    , Cmd.none
--                    )
                    ( model, Session.store newSession )

        ( GotSession newSession, _ ) -> --Redirect _ ) ->
            let
                _ = Debug.log "session" (toString newSession)
            in
            ( Redirect newSession
            , Cmd.none --Route.replaceUrl (Session.navKey session) Route.Home
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

        Login _ ->
            Page.view Page.Other Blank.view

        Home subModel ->
            viewPage Page.Home HomeMsg (Home.view subModel)

        Browse subModel ->
            Page.view Page.Browse (Browse.view subModel |> Html.map BrowseMsg)

        Search subModel ->
            Page.view Page.Search (Search.view subModel |> Html.map SearchMsg)

        Analyze subModel ->
            Page.view Page.Analyze (Analyze.view subModel |> Html.map AnalyzeMsg)

        App subModel ->
            Page.view Page.App (App.view subModel |> Html.map AppMsg)

        Project subModel ->
            Page.view Page.Project (Project.view subModel |> Html.map ProjectMsg)

        Sample subModel ->
            Page.view Page.Sample (Sample.view subModel |> Html.map SampleMsg)

        Campaign subModel ->
            Page.view Page.Campaign (Campaign.view subModel |> Html.map CampaignMsg)

        SamplingEvent subModel ->
            Page.view Page.SamplingEvent (SamplingEvent.view subModel |> Html.map SamplingEventMsg)

        Experiment subModel ->
            Page.view Page.Experiment (Experiment.view subModel |> Html.map ExperimentMsg)

        Contact subModel ->
            Page.view Page.Contact (Contact.view subModel |> Html.map ContactMsg)