module Page.Home exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Route exposing (Route)



-- VIEW --


view : Html msg
view =
    div [ class "fade-in" ]
        [ div [ class "jumbotron jumbotron-fluid", style "background-image" "url('assets/images/ocean-background.jpg')", style "background-size" "cover", style "min-height" "27em", style "border-bottom" "1px solid lightgray" ] --TODO move into css
            [ div [ class "container" ]
                [ h1 [ class "display-3 font-weight-bold", style "color" "#ff7a00", style "text-shadow" "2px 2px 3px rgba(0,0,0,0.6)" ] [ text "Planet Microbe" ]
                , p [ class "lead font-weight-bold", style "color" "#f0f0f0", style "font-size" "2em", style "text-shadow" "2px 2px 3px rgba(0,0,0,0.6)" ] [ text "Enabling the discovery and integration of oceanographic ‘omics, environmental and physiochemical data layers." ]
                , br [] []
                , a [ class "btn btn-light", Route.href Route.Search ] [ text "Try it now!" ]
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
                    ]
                , div [ class "col-lg-4" ]
                    [ img [ alt "Phase 2 Image", class "rounded-circle", attribute "height" "140", src "assets/images/temp_CDEBI.jpg", attribute "width" "140" ] []
                    , h2 [] [ text "Phase 2" ]
                    , h5 [] [ text "Interlinking Science Centers and Data Repositories" ]
                    , p []
                        [ text "Expand the prototype by adding other fundamental ‘omics data sets from NSF-funded Science and Technology Centers (STCs) including: C-DEBI and C-MORE by retrieving data from disparate ‘omic repositories (e.g., JGI/IMG, MG-RAST, NCBI, EBI) and reconnecting to environmental data in BCO-DMO using validated distance algorithms." ]
                    ]
                , div [ class "col-lg-4" ]
                    [ img [ alt "Phase 3 Image", class "rounded-circle", attribute "height" "140", src "assets/images/earthcube-projects.jpg", attribute "width" "140" ] []
                    , h2 [] [ text "Phase 3" ]
                    , h5 [] [ text "Incorporating EarthCube projects" ]
                    , p []
                        [ text "Leverage existing EarthCube funded projects to expand beyond data in BCO-DMO by integrating additional data and ontology from GeoLink, which also includes Rolling Deck to Repository (R2R) and International Ocean Discovery Program (IODP), linking and sharing resources with SeaView, and collaborating with GeoDeepDive for discovery of dark data in historical publications." ]
                    ]
                ]
            , div [ class "row" ]
                [ div [ class "col" ]
                    [ div [ class "text-center" ]
                        [ text "Planet Microbe is funded by NSF award "
                        , a [ href "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1639588&HistoricalAwards=false", target "_blank" ]
                            [ text "#1639588" ]
                        ]
                    ]
                ]
            , hr [ class "featurette-divider" ] []
            , div [ class "row featurette" ]
                [ div [ class "col-md-7" ]
                    [ h2 [ class "featurette-heading" ]
                        [ text "Semantic Search: "
                        , span [ class "text-muted" ] [ text "FAIR data in practice" ]
                        ]
                    , p [ class "lead" ]
                        [ text "Planet Microbe adheres to "
                        , a [ href "https://en.wikipedia.org/wiki/FAIR_data", target "_blank" ]
                            [ text "FAIR data principles" ]
                        , text " by using the "
                        , a [ href "http://environmentontology.org/", target "_blank" ]
                            [ text "EnvO Ontology" ]
                        , text " and "
                        , a [ href "http://www.obofoundry.org/", target "_blank" ]
                            [ text "OBO Foundry" ]
                        , text
                            """
                            to enable impactful multi-disciplinary analyses. All data are standardized, with
                            high-quality data and metadata linked to ontologies to enable advanced searches and
                            interoperability between heterogeneous datasets. Check out our
                            """
                        , a [ href "https://www.planetmicrobe.org/#/search" ]
                            [ text "metadata and 4D-search capabilities" ]
                        , text "."
                        ]
                    ]
                , div [ class "col-md-5" ]
                    [ a [ href "https://www.planetmicrobe.org/#/search" ]
                        [ img [ class "border shadow featurette-image img-fluid mx-auto", src "assets/images/search-demo.gif", alt "Animated GIF screen capture" ] []
                        ]
                    ]
                ]
            , hr [ class "featurette-divider" ] []
            , div [ class "row featurette" ]
                [ div [ class "col-md-7 order-md-2" ]
                    [ h2 [ class "featurette-heading" ]
                        [ text "Scalable Analytics: "
                        , span [ class "text-muted" ]
                            [ text "Innovating cyberinfrastructure design for scalable marine sciences" ]
                        ]
                    , p [ class "lead" ]
                        [ text
                            """
                            A tipping point for marine sciences is to enable ecosystem-level analyses through existing
                            computing and data resources, collaborative tool development, and large-scale oceanographic
                            ‘omics and environmental datasets. Planet Microbe uses existing data storage and
                            high-performance computational platforms available through
                            """
                        , a [ href "http://www.cyverse.org/", target "_blank" ]
                            [ text "CyVerse" ]
                        , text " and "
                        , a [ href "https://www.xsede.org/", target "_blank" ]
                            [ text "XSEDE" ]
                        , text "."
                        , text
                            """
                            The resulting Planet Microbe framework represents a community-driven and highly
                            accessible data architecture for optimizing global scale computation and expanding our
                            knowledge of microbial-driven ocean processes. New tools are added often,
                            check back with us regularly and follow us on
                            """
                        , a [ href "https://twitter.com/PlanetMicrobe", target "_blank" ]
                            [ text "Twitter" ]
                        , text "."
                        ]
                    ]
                , div [ class "col-md-5 order-md-1" ]
                    [ img [ class "border shadow featurette-image img-fluid mx-auto", src "assets/images/scalable-analytics.png", alt "architecture diagram" ]
                        []
                    ]
                ]
            , hr [ class "featurette-divider" ] []
            , div [ class "row featurette" ]
                [ div [ class "col-md-7" ]
                    [ h2 [ class "featurette-heading" ]
                        [ text "Community Integration: "
                        , span [ class "text-muted" ]
                            [ text "Collaboration and Outreach" ]
                        ]
                    , p [ class "lead" ]
                        [ text
                            """
                            Scientists, data scientists, and CI experts from diverse domains often “speak a different
                            language”. Planet Microbe connects with diverse scientists in the National Microbiome Data
                            Collaborative to share knowledge about how data might be used, by defining stories or use
                            cases. With guidance from the community, we develop data resources and tools that are
                            compatible with ideas and needs from the community. We actively recruit ideas from the
                            community through our feedback link, at conferences, and through our use-case protocols at
                            """
                        , a [ href "https://www.protocols.io/groups/hurwitz-lab", target "_blank" ]
                            [ text "Protocols.io" ]
                        , text "."
                        ]
                    ]
                , div [ class "col-md-5" ]
                    [ img [ class "border shadow featurette-image img-fluid mx-auto float-right", style "width" "80%", src "assets/images/earthcube-projects.jpg", alt "Diagram of Earthcube projects" ]
                        []
                    ]
                ]
            , hr [ class "featurette-divider" ] []
            ]
        , div [ style "background-image" "url('assets/images/ocean-floor.jpg')", style "background-size" "cover", style "min-height" "18em" ] --TODO move into css
            [ div [ class "container" ]
                [ div [ class "row", style "padding-top" "6em" ]
                    [ div [ class "col col-lg-5" ]
                        [ h2 [ class "font-weight-bold", style "text-shadow" "2px 2px 3px rgba(0,0,0,0.6)", style "font-size" "2.5em", style "color" "#F0F0F0" ]
                            [ text "Connect to" ]
                        , h2 [ class "font-weight-bold", style "text-shadow" "2px 2px 3px rgba(0,0,0,0.6)", style "font-size" "2.5em", style "color" "#F0F0F0" ]
                            [ text "Planet Microbe:" ]
                        ]
                    , div [ class "col", style "padding-top" "1em" ]
                        [ nav [ class "nav", style "text-shadow" "2px 2px 3px rgba(0,0,0,0.6)", style "font-size" "1.8em" ]
                            [ a [ class "nav-link", style "color" "#F0F0F0", href "https://twitter.com/PlanetMicrobe", target "_blank" ]
                                [ text "Twitter" ]
                            , a [ class "nav-link", style "color" "#F0F0F0", href "https://github.com/search?q=org%3Ahurwitzlab+planet-microbe&unscoped_q=planet-microbe", target "_blank" ]
                                [ text "GitHub" ]
                            , a [ class "nav-link", style "color" "#F0F0F0", href "http://www.hurwitzlab.org/", target "_blank" ]
                                [ text "Lab Website" ]
                            , a [ class "nav-link", style "color" "#F0F0F0", href "https://www.protocols.io/groups/hurwitz-lab", target "_blank" ]
                                [ text "Protocols.io" ]
                            ]
                        ]
                    ]
                ]
            ]
        , br [] []
        , footer []
            [ div [ class "container" ]
                [ div [ class "float-left text-muted small" ]
                    [ text "Copyright © 2020 Hurwitz Lab | All Rights Reserved" ]
                , div [ class "float-right" ]
                    [ a [ href "https://www.nsf.gov/", attribute "style" "padding-right:2em;", target "_blank" ]
                        [ img [ attribute "height" "60", src "assets/images/nsf-logo.jpg" ] [] ]
                    , a [ href "https://www.earthcube.org/", attribute "style" "padding-right:2em;", target "_blank" ]
                        [ img [ attribute "height" "50", src "assets/images/logo_earthcube_cube-only.png" ] [] ]
                    , a [ href "http://www.cyverse.org/", target "_blank" ]
                        [ img [ attribute "height" "50", src "assets/images/powered-by-cyverse-logo.png", attribute "width" "50" ] [] ]
                    ]
                , br [] []
                ]
            ]
        ]