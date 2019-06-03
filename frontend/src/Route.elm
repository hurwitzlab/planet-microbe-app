module Route exposing (Route(..), fromUrl, href, replaceUrl)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, int, string)
import Debug exposing (toString)



-- ROUTING


type Route
    = Home
    | Browse
    | Search
    | Analyze
    | Project Int
    | Sample Int
    | Campaign Int
    | SamplingEvent Int


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Browse (s "browse")
        , Parser.map Search (s "search")
        , Parser.map Analyze (s "analyze")
        , Parser.map Project (s "projects" </> int)
        , Parser.map Sample (s "samples" </> int)
        , Parser.map Campaign (s "campaigns" </> int)
        , Parser.map SamplingEvent (s "sampling_events" </> int)
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

                Browse ->
                    [ "browse" ]

                Search ->
                    [ "search" ]

                Analyze ->
                    [ "analyze" ]

                Project id ->
                    [ "projects", toString id ]

                Sample id ->
                    [ "samples", toString id ]

                Campaign id ->
                    [ "campaigns", toString id ]

                SamplingEvent id ->
                    [ "sampling_events", toString id ]
    in
    "#/" ++ String.join "/" pieces
