module Icon exposing (..)

{-| Font Awesome icons

This module was created to prevent the error below which occurs when Html.Attribute.class is used in an SVG element
instead of Svg.Attribute.class

    Cannot assign to read only property 'className' of object '#<SVGSVGElement>

IMPORTANT: do not import the Font Awesome javascript, it will cause runtime errors like:

    Uncaught TypeError: Cannot read property 'replaceData' of undefined

-}

import Html exposing (Html, i)
import Svg.Attributes exposing (class)



signIn : Html msg
signIn =
    i [ class "fas fa-fw fa-sign-in-alt" ] []


user : Html msg
user =
    i [ class "fas fa-fw fa-user" ] []


shoppingCart : Html msg
shoppingCart =
    i [ class "fas fa-fw fa-shopping-cart" ] []


shoppingCartLg : Html msg
shoppingCartLg =
    i [ class "fas fa-fw fa-shopping-cart fa-lg" ] []


exclamationTriangle : Html msg
exclamationTriangle =
    i [ class "fas fa-fw fa-exclamation-triangle" ] []


table : Html msg
table =
    i [ class "fa fa-fw fa-table" ] []


search : Html msg
search =
    i [ class "fa fa-fw fa-search" ] []


barChart : Html msg
barChart =
    i [ class "fa fa-fw fa-chart-bar" ] []


questionCircle : Html msg
questionCircle =
    i [ class "fa fa-fw fa-question-circle fa-lg" ] []


cog : Html msg
cog =
    i [ class "fas fa-fw fa-cog" ] []


cloud : Html msg
cloud =
    i [ class "fas fa-fw fa-cloud" ] []


cloudDownload : Html msg
cloudDownload =
    i [ class "fas fa-fw fa-cloud-download-alt" ] []


externalLink : Html msg
externalLink =
    i [ class "fas fa-fw fa-external-link-alt fa-xs" ] []


externalLinkSquare : Html msg
externalLinkSquare =
    i [ class "fas fa-fw fa-external-link-square-alt" ] []


file : Html msg
file =
    i [ class "fas fa-fw fa-file" ] []


fileDownload : Html msg
fileDownload =
    i [ class "fas fa-fw fa-file-download" ] []


folder : Html msg
folder =
    i [ class "fas fa-fw fa-folder" ] []


upload : Html msg
upload =
    i [ class "fas fa-fw fa-cloud-upload-alt" ] []


ban : Html msg
ban =
    i [ class "fas fa-fw fa-ban" ] []


plus : Html msg
plus =
    i [ class "fas fa-fw fa-plus" ] []


minus : Html msg
minus =
    i [ class "fas fa-fw fa-minus" ] []


plusSquare : Html msg
plusSquare =
    i [ class "far fa-fw fa-plus-square" ] []


minusSquare : Html msg
minusSquare =
    i [ class "far fa-fw fa-minus-square" ] []


trash : Html msg
trash =
    i [ class "fas fa-fw fa-trash" ] []


book : Html msg
book =
    i [ class "fas fa-fw fa-book" ] []


hierarchy : Html msg
hierarchy =
    i [ class "fas fa-fw fa-project-diagram" ] []
