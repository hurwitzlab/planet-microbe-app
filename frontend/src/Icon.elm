module Icon exposing (..)

{-| Font Awesome icons

This module was created to prevent the error below which occurs when Html.Attribute.class is used in an SVG element
instead of Svg.Attribute.class

    "Cannot assign to read only property 'className' of object '#<SVGSVGElement>"

-}

import Html exposing (Html, i)
import Svg.Attributes exposing (class)



signIn : Html msg
signIn =
    i [ class "fas fa-sign-in-alt" ] []


user : Html msg
user =
    i [ class "fas fa-user" ] []


shoppingCart : Html msg
shoppingCart =
    i [ class "fas fa-shopping-cart fa-lg" ] []


exclamationTriangle : Html msg
exclamationTriangle =
    i [ class "fas fa-exclamation-triangle" ] []


table : Html msg
table =
    i [ class "fa fa-table" ] []


search : Html msg
search =
    i [ class "fa fa-search" ] []


barChart : Html msg
barChart =
    i [ class "fa fa-chart-bar" ] []


questionCircle : Html msg
questionCircle =
    i [ class "fa fa-question-circle fa-lg" ] []


cog : Html msg
cog =
    i [ class "fas fa-cog" ] []


cloud : Html msg
cloud =
    i [ class "fas fa-cloud" ] []


externalLink : Html msg
externalLink =
    i [ class "fas fa-external-link-alt fa-xs" ] []


file : Html msg
file =
    i [ class "fas fa-file" ] []


ban : Html msg
ban =
    i [ class "fas fa-ban" ] []
