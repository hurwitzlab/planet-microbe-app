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
--    | Root
--    | Login
--    | Logout
--    | Register
--    | Settings
--    | Article Slug
--    | Profile Username
--    | NewArticle
--    | EditArticle Slug


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Browse (s "browse")
        , Parser.map Search (s "search")
        , Parser.map Analyze (s "analyze")
        , Parser.map Project (s "project" </> int)
        , Parser.map Sample (s "sample" </> int)
--        , Parser.map Login (s "login")
--        , Parser.map Logout (s "logout")
--        , Parser.map Settings (s "settings")
--        , Parser.map Profile (s "profile" </> Username.urlParser)
--        , Parser.map Register (s "register")
--        , Parser.map Article (s "article" </> Slug.urlParser)
--        , Parser.map NewArticle (s "editor")
--        , Parser.map EditArticle (s "editor" </> Slug.urlParser)
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
                    [ "project", toString id ]

                Sample id ->
                    [ "sample", toString id ]

--                Root ->
--                    []
--
--                Login ->
--                    [ "login" ]
--
--                Logout ->
--                    [ "logout" ]
--
--                Register ->
--                    [ "register" ]
--
--                Settings ->
--                    [ "settings" ]
--
--                Article slug ->
--                    [ "article", Slug.toString slug ]
--
--                Profile username ->
--                    [ "profile", Username.toString username ]
--
--                NewArticle ->
--                    [ "editor" ]
--
--                EditArticle slug ->
--                    [ "editor", Slug.toString slug ]
    in
    "#/" ++ String.join "/" pieces
