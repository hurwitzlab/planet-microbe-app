module Page.Home exposing (Model, Msg, init, update, view)

--import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Encode as Encode
--import Page.Error as Error exposing (PageLoadError)
import Task exposing (Task)



---- MODEL ----


type alias Model =
    { pageTitle : String
    , pageBody : String
    }


init : ( Model, Cmd Msg )
init =
    ( { pageTitle = "", pageBody = "" }, Cmd.none )



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
    div [ class "fade-in" ]
        [ div [ class "jumbotron jumbotron-fluid", style "background-image" "url('assets/images/ocean-background.jpg')", style "background-size" "cover", style "min-height" "27em", style "border-bottom" "1px solid lightgray" ] --TODO move into css
            [ div [ class "container" ]
                [ h1 [ class "display-3 font-weight-bold", style "color" "#ff7a00", style "text-shadow" "2px 2px 3px rgba(0,0,0,0.6)" ] [ text "Planet Microbe" ]
                , p [ class "lead font-weight-bold", style "color" "#f0f0f0", style "font-size" "2em", style "text-shadow" "2px 2px 3px rgba(0,0,0,0.6)" ] [ text "Enabling the discovery and integration of oceanographic ‘omics, environmental and physiochemical data layers." ]
                ]
            ]
        , br [] []
        , div [ class "container marketing" ]
            [ div [ class "row", style "color" "#5a5a5a" ]
                [ div [ class "col-lg-4" ]
                    [ img [ alt "Phase 1 Image", class "rounded-circle", attribute "height" "140", src "assets/images/KM2_clemente.jpg", attribute "width" "140" ] []
                    , h2 [] [ text "Phase 1" ]
                    , h5 [] [ text "Integrating Oceanographic Time Series" ]
                    , p []
                        [ text " Develop a working prototype that will connect ‘omics data (available via iMicrobe) to the data-rich, critically important oceanographic time series: HOT and BATS by linking their environmental data (available via BCO-DMO) to the ‘omics data in iMicrobe. This linkage will greatly improve current and future collaborations between oceanographers and geoscientists by providing highly-curated, integrated data sets." ]
                    , p []
                        [ a [ class "btn btn-secondary", href "#", attribute "role" "button" ] [ text "View details »" ]
                        ]
                    ]
                , div [ class "col-lg-4" ]
                    [ img [ alt "Phase 2 Image", class "rounded-circle", attribute "height" "140", src "assets/images/temp_CDEBI.jpg", attribute "width" "140" ] []
                    , h2 [] [ text "Phase 2" ]
                    , h5 [] [ text "Interlinking Science Centers and Data Repositories" ]
                    , p []
                        [ text "Expand the prototype by adding other fundamental ‘omics data sets from NSF-funded Science and Technology Centers (STCs) including: C-DEBI and C-MORE by retrieving data from disparate ‘omic repositories (e.g., JGI/IMG, MG-RAST, NCBI, EBI) and reconnecting to environmental data in BCO-DMO using validated distance algorithms." ]
                    , p []
                        [ a [ class "btn btn-secondary", href "#", attribute "role" "button" ] [ text "View details »" ]
                        ]
                    ]
                , div [ class "col-lg-4" ]
                    [ img [ alt "Phase 3 Image", class "rounded-circle", attribute "height" "140", src "assets/images/earthcube-projects.jpg", attribute "width" "140" ] []
                    , h2 [] [ text "Phase 3" ]
                    , h5 [] [ text "Incorporating EarthCube projects" ]
                    , p []
                        [ text "Leverage existing EarthCube funded projects to expand beyond data in BCO-DMO by integrating additional data and ontology from GeoLink, which also includes Rolling Deck to Repository (R2R) and International Ocean Discovery Program (IODP), linking and sharing resources with SeaView, and collaborating with GeoDeepDive for discovery of dark data in historical publications." ]
                    , p []
                        [ a [ class "btn btn-secondary", href "#", attribute "role" "button" ] [ text "View details »" ]
                        ]
                    ]
                , text "        "
                ]
            , hr [ class "featurette-divider" ] []
            , div [ class "row featurette" ]
                [ div [ class "col-md-7" ]
                    [ h2 [ class "featurette-heading" ]
                        [ text "Semantic Search. "
                        , span [ class "text-muted" ]
                            [ text "It'll blow your mind." ]
                        ]
                    , p [ class "lead" ]
                        [ text "Donec ullamcorper nulla non metus auctor fringilla. Vestibulum id ligula porta felis euismod semper. Praesent commodo cursus magna, vel scelerisque nisl consectetur. Fusce dapibus, tellus ac cursus commodo." ]
                    ]
                , div [ class "col-md-5" ]
                    [ img [ alt "Animated GIF screen capture", class "featurette-image img-fluid mx-auto", attribute "data-src" "holder.js/500x500/auto" ]
                        []
                    ]
                ]
            , hr [ class "featurette-divider" ] []
            , div [ class "row featurette" ]
                [ div [ class "col-md-7 order-md-2" ]
                    [ h2 [ class "featurette-heading" ]
                        [ text "Scalable Analytics. "
                        , span [ class "text-muted" ]
                            [ text "See for yourself." ]
                        ]
                    , p [ class "lead" ]
                        [ text "Donec ullamcorper nulla non metus auctor fringilla. Vestibulum id ligula porta felis euismod semper. Praesent commodo cursus magna, vel scelerisque nisl consectetur. Fusce dapibus, tellus ac cursus commodo." ]
                    ]
                , div [ class "col-md-5 order-md-1" ]
                    [ img [ alt "Animated GIF screen capture", class "featurette-image img-fluid mx-auto", attribute "data-src" "holder.js/500x500/auto" ]
                        []
                    ]
                ]
            , hr [ class "featurette-divider" ] []
            , div [ class "row featurette" ]
                [ div [ class "col-md-7" ]
                    [ h2 [ class "featurette-heading" ]
                        [ text "And lastly, this one. "
                        , span [ class "text-muted" ]
                            [ text "Checkmate." ]
                        ]
                    , p [ class "lead" ]
                        [ text "Donec ullamcorper nulla non metus auctor fringilla. Vestibulum id ligula porta felis euismod semper. Praesent commodo cursus magna, vel scelerisque nisl consectetur. Fusce dapibus, tellus ac cursus commodo." ]
                    ]
                , div [ class "col-md-5" ]
                    [ img [ alt "Animated GIF screen capture", class "featurette-image img-fluid mx-auto", attribute "data-src" "holder.js/500x500/auto" ]
                        []
                    ]
                ]
            , hr [ class "featurette-divider" ] []
            ]
        , div [ style "background-image" "url('assets/images/ocean-floor.jpg')", style "background-size" "cover", style "min-height" "18em" ] --TODO move into css
            [ div [ class "container" ]
                [ div [ class "row", style "padding-top" "7.2em" ]
                    [ div [ class "col col-lg-7" ]
                        [ h2 [ class "font-weight-bold", style "text-shadow" "2px 2px 3px rgba(0,0,0,0.6)", style "font-size" "2.5em", style "color" "#F0F0F0" ]
                            [ text "Connect to Planet Microbe:" ]
                        ]
                    , div [ class "col" ]
                        [ nav [ class "nav" ]
                            [ a [ class "nav-link", href "https://members.aaas.org/trelliscommunitiesbyaaas/communities/community-home?CommunityKey=c9a11f04-f616-4c64-a216-a9e7fe4221e5", target "_blank" ]
                                [ text "Trellis Blog" ]
                            , a [ class "nav-link", href "http://www.hurwitzlab.org/", target "_blank" ]
                                [ text "Lab Website" ]
                            , a [ class "nav-link", href "https://twitter.com/PlanetMicrobe", target "_blank" ]
                                [ text "Twitter" ]
                            , a [ class "nav-link", href "https://github.com/search?q=org%3Ahurwitzlab+planet-microbe&unscoped_q=planet-microbe", target "_blank" ]
                                [ text "GitHub" ]
                            ]
                        ]
                    ]
                ]
            ]
        , br [] []
        , footer []
            [ div [ class "container" ]
                [ div [ class "float-left text-muted small" ]
                    [ text "Copyright © 2019 Hurwitz Lab | All Rights Reserved" ]
                , div [ class "float-right" ]
                    [ a [ href "https://www.nsf.gov/", attribute "style" "padding-right:2em;", target "_blank" ]
                        [ img [ attribute "height" "60", src "assets/images/nsf-logo.jpg" ] [] ]
                    , a [ href "https://www.earthcube.org/", attribute "style" "padding-right:2em;", target "_blank" ]
                        [ img [ attribute "height" "50", src "assets/images/logo_earthcube_cube-only.png" ] [] ]
                    , a [ href "http://www.cyverse.org/", target "_blank" ]
                        [ img [ attribute "height" "50", src "assets/images/powered-by-cyverse-logo.png", attribute "width" "50" ] [] ]
                    ]
                ]
            ]
        ]