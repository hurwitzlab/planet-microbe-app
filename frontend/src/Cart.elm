port module Cart exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)
import Set exposing (Set)
import Html exposing (Html, button, th, td, tr, table, thead, tbody, text, a, input)
import Html.Attributes exposing (class, type_, checked, href, target)
import Html.Events exposing (onClick)
import Icon
import Route
import Config exposing (dataCommonsUrl, sraUrl)



-- TYPES --


type Cart =
    Cart Model


type alias Model =
    { contents : Set Int -- Set of IDs for all files in cart
    , selected : Set Int -- Set of IDs for selected files in cart
    }


type CartType
    = Selectable
    | Editable



-- SERIALIZATION --


decoder : Decoder Cart
decoder =
    Decode.succeed Model
        |> required "contents" (Decode.list Decode.int |> Decode.map Set.fromList)
        |> optional "selected" (Decode.list Decode.int |> Decode.map Set.fromList) Set.empty
        |> Decode.map Cart


encode : Cart -> Value
encode (Cart cart) =
    Encode.object
        [ ( "contents", cart.contents |> Set.toList |> Encode.list Encode.int )
        ]


store : Cart -> Cmd msg
store cart =
    encode cart
        |> Just
        |> storeCart


port storeCart : Maybe Value -> Cmd msg


port onCartChange : (String -> msg) -> Sub msg



-- UTILITY FUNCTIONS --


empty : Cart
empty =
    Cart (Model Set.empty Set.empty)


size : Cart -> Int
size (Cart cart) =
    Set.size cart.contents


toList : Cart -> List Int
toList (Cart cart) =
    cart.contents |> Set.toList


contains : Cart -> Int -> Bool
contains (Cart cart) id =
    Set.member id cart.contents


add : Cart -> List Int -> Cart
add (Cart cart) ids =
    Cart { cart | contents = Set.union (Set.fromList ids) cart.contents }


remove : Cart -> List Int -> Cart
remove (Cart cart) ids =
    Cart { cart | contents = Set.diff cart.contents (Set.fromList ids) }


selected : Cart -> Int -> Bool
selected (Cart cart) id =
    Set.member id cart.selected


selectedToList : Cart -> List Int
selectedToList (Cart cart) =
    cart.selected |> Set.toList


select : Cart -> Int -> Cart
select (Cart cart) id =
    Cart { cart | selected = Set.insert id cart.selected }


selectAll : Cart -> Cart
selectAll (Cart cart) =
    Cart { cart | selected = cart.contents }


unselect : Cart -> Int -> Cart
unselect (Cart cart) id =
    Cart { cart | selected = Set.remove id cart.selected }


unselectAll : Cart -> Cart
unselectAll (Cart cart) =
    Cart { cart | selected = Set.empty }



-- UPDATE --


type Msg
    = AddToCart (List Int)
    | RemoveFromCart (List Int)
    | ToggleSelectInCart Int
    | SelectAllInCart
    | UnselectAllInCart


update : Msg -> Cart -> Cart
update msg cart =
    case msg of
        AddToCart idList ->
            add cart idList

        RemoveFromCart idList ->
            remove cart idList

        ToggleSelectInCart id ->
            if selected cart id then
                unselect cart id
            else
                select cart id

        SelectAllInCart ->
            selectAll cart

        UnselectAllInCart ->
            unselectAll cart



-- VIEW --


view : Cart ->
    List
        { a | id : Int
        , url : String
        , sampleId : Int
        , sampleAccn : String
        , experimentId : Int
        , experimentAccn : String
        , source : String
        , layout : String
        , runAccn : String
        , projectId : Int
        , projectName : String
        } -> CartType -> Html Msg
view cart files cartType =
    let
        row file =
            let
                basename path =
                    String.split "/" path |> List.reverse |> List.head |> Maybe.withDefault ""
            in
            tr []
                [ if cartType == Selectable then
                    td []
                        [ input [ type_ "checkbox", checked (selected cart file.id), onClick (ToggleSelectInCart file.id) ] [] ]
                  else
                    text "" --td [] []
                , td [] [ a [ href (dataCommonsUrl ++ file.url), target "_blank" ] [ text <| basename file.url ] ]
                , td [] [ text <| file.source ++ "/" ++ file.layout ]
                , td [] [ a [ href (sraUrl ++ file.runAccn), target "_blank" ] [ text file.runAccn ] ]
                , td [] [ a [ Route.href (Route.Experiment file.experimentId) ] [ text file.experimentAccn ] ]
                , td [] [ a [ Route.href (Route.Sample file.sampleId) ] [ text file.sampleAccn ] ]
                , td [  class "text-nowrap" ] [ a [ Route.href (Route.Project file.projectId) ] [ text file.projectName ] ]
                , if cartType == Editable then
                    td []
                        [ button [ class "btn btn-outline-secondary btn-sm float-right", onClick (RemoveFromCart [ file.id ]) ] [ text "Remove" ] ]
                  else
                    td [] []
                ]
    in
    table [ class "table" ]
        [ thead []
            [ if cartType == Selectable then
                th [] []
              else
                text ""
            , th [] [ text "File ", Icon.externalLink ]
            , th [] [ text "Source/Layout" ]
            , th [] [ text "Run ", Icon.externalLink ]
            , th [] [ text "Experiment" ]
            , th [] [ text "Sample" ]
            , th [] [ text "Project" ]
            , th [] []
            ]
        , tbody []
            (List.map row files) --(samplesInCart cart samples |> List.map row)
        ]


addToCartButton : Cart -> Maybe (String, String) -> List Int -> Html Msg
addToCartButton cart optionalLabels idList =
    let
        (addLbl, removeLbl) =
            optionalLabels |> Maybe.withDefault ( "Add", "Remove" )

        btn label clickMsg =
            button [ class "btn btn-xs btn-outline-secondary text-nowrap", onClick clickMsg ]
                [ text label ]
    in
    if List.any (\id -> contains cart id) idList then
        btn removeLbl (RemoveFromCart idList)
    else
        btn addLbl (AddToCart idList)


addAllToCartButton : Cart -> Maybe (String, String) -> List Int -> Html Msg
addAllToCartButton cart optionalLabels idList =
    let
        (addLbl, removeLbl) =
            optionalLabels |> Maybe.withDefault ( "Add All", "Remove All" )

        btn label clickMsg =
            button [ class "btn btn-xs btn-outline-secondary align-middle text-nowrap", onClick clickMsg ]
                [ Icon.shoppingCart
                , text " "
                , text label
                ]
    in
    if List.any (\id -> contains cart id) idList then
        btn removeLbl (RemoveFromCart idList)
    else
        btn addLbl (AddToCart idList)
