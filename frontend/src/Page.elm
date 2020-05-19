module Page exposing (Page(..), view, viewErrors, viewRemoteData, viewTitle, viewTitle1, viewTitle2, viewBlank, viewSpinner, viewSpinnerCentered, viewSpinnerOverlay, viewSpinnerOverlayCentered, viewToggleText, viewDialog, viewMessageDialog, viewJobStatus)

import Browser exposing (Document)
import Html exposing (Html, a, button, h1, h2, h5, div, span, img, i, li, nav, p, text, ul, small, footer)
import Html.Attributes exposing (id, class, classList, src, href, style, title, type_, attribute, tabindex, target)
import Html.Events exposing (onClick)
import Http
import RemoteData exposing (RemoteData(..))
import Error
import Icon
import Route exposing (Route)
import Session exposing (Session)
import Cart
import Config


{-| Determines which navbar link (if any) will be rendered as active.

Note that we don't enumerate every page here, because the navbar doesn't
have links for every page. Anything that's not part of the navbar falls
under Other.

-}
type Page
    = Other
    | Home
    | Maintenance
    | Browse
    | Search
    | Analyze
    | App
    | Job
    | Project
    | Sample
    | Campaign
    | SamplingEvent
    | Experiment
    | Contact
    | Account
    | Cart


{-| Take a page's Html and frames it with a header and footer.

The caller provides the current user, so we can display in either
"signed in" (rendering username) or "signed out" mode.

isLoading is for determining whether we should show a loading spinner
in the header. (This comes up during slow page transitions.)

-}
--view : Maybe Viewer -> Page -> { title : String, content : Html msg } -> Document msg
--view maybeViewer page { title, content } =
--    { title = title ++ " - Conduit"
--    , body = viewHeader page maybeViewer :: content --:: [ viewFooter ]
--    }


view : Session -> Page -> Html msg -> Document msg
view session page content =
    { title = "Planet Microbe"
    , body =
        [ viewHeader session page
--        , br [] []
        , content
        , viewFooter
        ]
    }


viewHeader : Session -> Page -> Html msg
viewHeader session page =
    let
        loginButton =
            case session of
                Session.Guest _ _ _ ->
                    a [ class "nav-link text-nowrap", Route.href Route.Login ]
                        [ Icon.signIn
                        , text " Sign-in to CyVerse"
                        ]

                Session.LoggedIn _ _ _ _ ->
                    a [ class "nav-link text-nowrap", classList [ ("active", page == Account) ], Route.href Route.Account ]
                        [ Icon.user
                        , text " My Account"
                        ]

        cartButton =
            let
                numItemsInCart =
                    Cart.size (Session.getCart session)

                label =
                    if numItemsInCart == 0 then
                        ""
                    else
                        String.fromInt numItemsInCart
            in
            a [ class "nav-link text-nowrap", classList [ ("active", page == Cart) ], style "min-width" "4em", Route.href Route.Cart ]
                [ Icon.shoppingCartLg
                , text " "
                , span [ class "gray absolute" ] [ text label ]
                ]
    in
    div []
        [ (case Config.alertBannerText of
            Nothing ->
                text ""

            Just alertBannerText ->
                div [ class "text-center border-bottom", style "color" "gray", style "background-color" "PowderBlue" ]
                    [ Icon.exclamationTriangle
                    , text " "
                    , text alertBannerText
                    ]
          )
        , div [ style "background-color" "#f8f8f8", style "border-bottom" "1px solid lightgray" ]
            [ nav [ class "navbar navbar-expand-lg navbar-light bg-light" ]
                [ a [ class "navbar-brand", style "margin-right" "5em", Route.href Route.Home ]
                    [ img [ src "assets/images/pm-logo.png", style "width" "238px", style "height" "49px" ] [] ]
                , div [ class "navbar-collapse collapse", id "navbarNav" ]
                    [ ul [ class "navbar-nav" ]
                        [ li [ class "nav-item", style "width" "9em" ]
                            [ a [ class "nav-link", classList [ ("active", page == Browse) ], Route.href Route.Browse ]
                                [ Icon.table
                                , text " Browse"
                                ]
                            ]
                        , li [ class "nav-item", style "width" "9em" ]
                            [ a [ class "nav-link", classList [ ("active", page == Search) ], Route.href Route.Search ]
                                [ Icon.search
                                , text " Search"
                                ]
                            ]
                        , li [ class "nav-item", style "width" "9em" ]
                            [ a [ class "nav-link", classList [ ("active", page == Analyze) ], Route.href (Route.Analyze Nothing)  ]
                                [ Icon.barChart
                                , text " Analyze"
                                ]
                            ]
                        , li [ class "nav-item", style "width" "10em" ]
                            [ a [ class "nav-link", href "https://hurwitzlab.gitbook.io/planet-microbe-documentation/", target "_blank" ]
                                [ Icon.book
                                , text " Documentation"
                                ]
                            ]
                        ]
                    , ul [ class "navbar-nav ml-auto" ]
                        [ li [ class "nav-item mr-4" ]
                            [ loginButton ]
                        , li [ class "nav-item ml-2 mr-4" ]
                            [ cartButton]
                        , li [ class "nav-item mr-4" ]
                            [ a [ class "nav-link", title "Get Help", Route.href Route.Contact ]
                                [ Icon.questionCircle ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewFooter : Html msg
viewFooter =
    footer [ style "min-height" "3em" ] []


--navbarLink : Page -> Route -> List (Html msg) -> Html msg
--navbarLink page route linkContent =
--    li [ classList [ ( "nav-item", True ), ( "active", isActive page route ) ] ]
--        [ a [ class "nav-link", Route.href route ] linkContent ]


--isActive : Page -> Route -> Bool
--isActive page route =
--    case ( page, route ) of
--        ( Home, Route.Home ) ->
--            True
--
--        ( Login, Route.Login ) ->
--            True
--
--        ( Register, Route.Register ) ->
--            True
--
--        ( Settings, Route.Settings ) ->
--            True
--
--        ( Profile pageUsername, Route.Profile routeUsername ) ->
--            pageUsername == routeUsername
--
--        ( NewArticle, Route.NewArticle ) ->
--            True
--
--        _ ->
--            False


{-| Render dismissable errors. We use this all over the place!
-}
viewErrors : msg -> List String -> Html msg
viewErrors dismissErrors errors =
    if List.isEmpty errors then
        Html.text ""

    else
        div
            [ class "error-messages"
            , style "position" "fixed"
            , style "top" "0"
            , style "background" "rgb(250, 250, 250)"
            , style "padding" "20px"
            , style "border" "1px solid"
            ]
        <|
            List.map (\error -> p [] [ text error ]) errors
                ++ [ button [ onClick dismissErrors ] [ text "Ok" ] ]


viewRemoteData : RemoteData Http.Error a -> (a -> Html msg) -> Html msg
viewRemoteData remoteData viewDataFunc =
    case remoteData of
        Success data ->
            viewDataFunc data

        Loading ->
            viewSpinner

        Failure error ->
            Error.view error False

        NotAsked ->
            viewBlank


viewTitle : String -> String -> Html msg
viewTitle title subTitle =
    h1 [ class "pb-2 mt-5 mb-2 font-weight-bold border-bottom", style "width" "100%" ]
        [ span [ style "color" "dimgray" ] [ text title ]
        , small [ class "ml-3", style "color" "gray" ] [ text subTitle ]
        ]


viewTitle1 : String -> Bool -> Html msg
viewTitle1 title border =
    h1 [ class "pb-2 mt-4 mb-2 font-weight-bold align-middle d-inline", classList [("border-bottom", border)], style "color" "dimgray" ]
        [ text title ]


viewTitle2 : String -> Bool -> Html msg
viewTitle2 title border =
    h2 [ class "pb-2 mt-4 mb-2 font-weight-bold align-middle d-inline", classList [("border-bottom", border)], style "color" "dimgray" ]
        [ text title ]


viewBlank : Html msg
viewBlank =
    text ""


viewSpinner : Html msg
viewSpinner =
    div [ class "d-flex justify-content-center m-5" ]
        [ div [ class "d-flex spinner-border", style "color" "lightgray", style "width" "4rem", style "height" "4rem" ]
            [ span [ class "sr-only" ] [ text "Loading..." ]
            ]
        ]


viewSpinnerCentered : Html msg
viewSpinnerCentered =
    div [ class "d-flex justify-content-center", style "margin-top" "40vh" ]
        [ div [ class "d-flex spinner-border", style "margin" "0 auto", style "color" "lightgray", style "width" "4rem", style "height" "4rem"  ]
            [ span [ class "sr-only" ] [ text "Loading..." ]
            ]
        ]


viewSpinnerOverlay : String -> Bool -> Html msg
viewSpinnerOverlay offset dimBg =
    div ((if dimBg then
             style "background-color" "rgba(0,0,0,0.3)"
         else
             style "" ""
        ) ::
        [ style "position" "fixed"
        , style "width" "100%"
        , style "height" "100%"
        , style "top" "0"
        , style "left" "0"
        , style "right" "0"
        , style "bottom" "0"
        , style "z-index" "2"
        ])
        [ div [ style "padding-top" offset ] [ viewSpinner ] ]


viewSpinnerOverlayCentered : Html msg
viewSpinnerOverlayCentered =
    viewSpinnerOverlay "40vh" True


viewToggleText : String -> Bool -> msg -> Html msg
viewToggleText s expanded toggleMsg =
    let
        maxLength = 200
    in
    if String.length s < maxLength || expanded then
        text s
    else
        span []
            [ String.slice 0 maxLength s |> text
            , text "... "
            , a [ href "", onClick toggleMsg ] [ text "more" ]
            ]


-- This is our own Boostrap modal since elm-dialog has not yet been ported to Elm 0.19
viewDialog : String -> List (Html msg) -> List (Html msg) -> msg -> Html msg
viewDialog title body footer closeMsg =
    div []
        [ div [ class "modal fade show", tabindex -1, style "display" "block", attribute "role" "dialog" ]
            [ div [ class "modal-dialog", attribute "role" "document", style "min-width" "35em" ]
                [ div [ class "modal-content" ]
                    [ div [ class "modal-header" ]
                        [ h5 [ class "modal-title" ] [ text title ]
                        , button [ type_ "button", class "close", onClick closeMsg ] [ span [] [ text (String.fromChar (Char.fromCode 215)) ] ]
                        ]
                    , div [ class "modal-body" ] body
                    , div [ class "modal-footer" ] footer
                    ]
                ]
            ]
        , div [ class "modal-backdrop fade show" ] []
        ]


viewMessageDialog : String -> msg -> Html msg
viewMessageDialog message closeMsg =
    viewDialog "Note"
        [ div [ style "overflow-y" "auto", style "max-height" "50vh", style "text-align" "center", style "margin" "2em" ]
            [ text message ]
        ]
        [ button [ type_ "button", class "btn btn-secondary", onClick closeMsg ]
            [ text "Close" ]
        ]
        closeMsg


viewJobStatus : String -> Html msg
viewJobStatus status =
    let
        label =
            String.replace "_" " " status -- replace _ with space

        color =
            case String.toUpper label of
                "FINISHED" -> "text-primary"
                "FAILED" -> "text-danger"
                _ -> "text-secondary"
    in
    span [ class color ] [ text label ]
