module Route exposing (Route(..), fromUrl, href, replaceUrl, routeToString)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, int, string)



-- ROUTING


type Route
    = Home
    | Login
    | Logout
    | Browse
    | Search
    | Analyze
    | App Int
    | Job String
    | Project Int
    | Sample Int
    | Campaign Int
    | SamplingEvent Int
    | Experiment Int
    | Contact
    | Account
    | Cart


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        , Parser.map Logout (s "logout")
        , Parser.map Browse (s "browse")
        , Parser.map Search (s "search")
        , Parser.map Analyze (s "analyze")
        , Parser.map App (s "apps" </> int)
        , Parser.map Job (s "jobs" </> string)
        , Parser.map Project (s "projects" </> int)
        , Parser.map Sample (s "samples" </> int)
        , Parser.map Campaign (s "campaigns" </> int)
        , Parser.map SamplingEvent (s "sampling_events" </> int)
        , Parser.map Experiment (s "experiments" </> int)
        , Parser.map Contact (s "contact")
        , Parser.map Account (s "account")
        , Parser.map Cart (s "cart")
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser



-- INTERNAL


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                Login ->
                    [ "login" ]

                Logout ->
                    [ "logout" ]

                Browse ->
                    [ "browse" ]

                Search ->
                    [ "search" ]

                Analyze ->
                    [ "analyze" ]

                App id ->
                    [ "apps", String.fromInt id ]

                Job id ->
                    [ "jobs", id ]

                Project id ->
                    [ "projects", String.fromInt id ]

                Sample id ->
                    [ "samples", String.fromInt id ]

                Campaign id ->
                    [ "campaigns", String.fromInt id ]

                SamplingEvent id ->
                    [ "sampling_events", String.fromInt id ]

                Experiment id ->
                    [ "experiments", String.fromInt id ]

                Contact ->
                    [ "contact" ]

                Account ->
                    [ "account" ]

                Cart ->
                    [ "cart" ]
    in
    "#/" ++ String.join "/" pieces
