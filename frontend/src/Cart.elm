port module Cart exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)
import Set exposing (Set)
import Html exposing (..)
import Html.Attributes exposing (class, type_, checked, href, target)
import Html.Events exposing (onClick)
import Icon
import Route
import Config exposing (sraUrl)



-- TYPES


type Cart =
    Cart Model


type alias Model =
    { contents : Set Int -- Set of IDs for all files in cart
    , selected : Set Int -- Set of IDs for selected files in cart
    }


type CartType
    = Selectable
    | Editable



-- SERIALIZATION


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



-- UTILITY FUNCTIONS


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


selectList : Cart -> List Int -> Cart
selectList (Cart cart) ids =
    Cart { cart | selected = Set.union (Set.fromList ids) cart.selected }


unselect : Cart -> Int -> Cart
unselect (Cart cart) id =
    Cart { cart | selected = Set.remove id cart.selected }


unselectAll : Cart -> Cart
unselectAll (Cart cart) =
    Cart { cart | selected = Set.empty }


unselectList : Cart -> List Int -> Cart
unselectList (Cart cart) ids =
    Cart { cart | selected = Set.diff cart.selected (Set.fromList ids) }



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


--tableConfig : Model -> Table.Config { a | sample_id : Int, sample_name : String, project : { b | project_id : Int, project_name : String } } Msg
--tableConfig model =
--    let
--        columns =
--            case model.cartType of
--                Editable ->
--                    [ projectColumn
--                    , nameColumn
--                    , removeFromCartColumn
--                    ]
--
--                Selectable ->
--                    [ selectInCartColumn model
--                    , projectColumn
--                    , nameColumn
--                    ]
--    in
--    Table.customConfig
--        { toId = toString << .sample_id
--        , toMsg = SetTableState
--        , columns = columns
--        , customizations =
--            { defaultCustomizations | tableAttrs = toTableAttrs }
--        }
--
--
--toTableAttrs : List (Attribute Msg)
--toTableAttrs =
--    [ attribute "class" "table"
--    ]


view : Cart -> List { a | id : Int, url : String, sampleId : Int, sampleAccn : String, experimentId : Int, experimentAccn : String, runAccn : String, projectId : Int, projectName : String } -> CartType -> Html Msg
view cart files cartType =
--    Table.view (tableConfig model) model.tableState (samplesInCart model.cart samples)
    let
        row file =
            tr []
                [ if cartType == Selectable then
                    td []
                        [ input [ type_ "checkbox", checked (selected cart file.id), onClick (ToggleSelectInCart file.id) ] [] ]
                  else
                    td [] []
                , td [] [ a [ Route.href (Route.Project file.projectId) ] [ text file.projectName ] ]
                , td [] [ a [ Route.href (Route.Sample file.sampleId) ] [ text file.sampleAccn ] ]
                , td [] [ a [ Route.href (Route.Experiment file.experimentId) ] [ text file.experimentAccn ] ]
                , td [] [ a [ href (sraUrl ++ file.runAccn), target "_blank" ] [ text file.runAccn ] ]
                , td [] [ a [ href file.url, target "_blank" ] [ text file.url ] ]
                , if cartType == Editable then
                    td []
                        [ button [ class "btn btn-outline-secondary btn-sm float-right", onClick (RemoveFromCart [ file.id ]) ] [ text "Remove" ] ]
                  else
                    td [] []
                ]
    in
    table [ class "table" ]
        [ thead []
            [ th [] []
            , th [] [ text "Project" ]
            , th [] [ text "Sample" ]
            , th [] [ text "Experiment" ]
            , th [] [ text "Run ", Icon.externalLink ]
            , th [] [ text "Link ", Icon.externalLink ]
            , th [] []
            ]
        , tbody []
            (List.map row files) --(samplesInCart cart samples |> List.map row)
        ]


--selectInCartColumn : Model -> Table.Column { a | sample_id : Int, sample_name : String } Msg
--selectInCartColumn model =
--    Table.veryCustomColumn
--        { name = ""
--        , viewData = (\s -> selectInCartLink model s)
--        , sorter = Table.unsortable
--        }
--
--
--selectInCartLink : Model -> { a | sample_id : Int, sample_name : String } -> Table.HtmlDetails Msg
--selectInCartLink model sample =
--    let
--        isChecked =
--            Set.member sample.sample_id model.selected.contents
--    in
--    Table.HtmlDetails []
--        [ selectInCartCheckbox sample.sample_id isChecked -- |> Html.map (\_ -> ToggleSelectInCart sample.sample_id)
--        ]
--
--
--selectInCartCheckbox : Int -> Bool -> Html Msg
--selectInCartCheckbox id isChecked =
--    input [ type_ "checkbox", checked isChecked, onClick (ToggleSelectInCart id) ] []
--
--
--projectColumn : Table.Column { a | sample_id : Int, sample_name : String, project : { b | project_id : Int, project_name : String } } Msg
--projectColumn =
--    Table.veryCustomColumn
--        { name = "Project"
--        , viewData = projectLink
--        , sorter = Table.increasingOrDecreasingBy (.project >> .project_name >> String.toLower)
--        }
--
--
--projectLink : { a | sample_id : Int, sample_name : String, project : { b | project_id : Int, project_name : String } } -> Table.HtmlDetails Msg
--projectLink sample =
--    Table.HtmlDetails []
--        [ a [ Route.href (Route.Project sample.project.project_id) ]
--            [ text <| Util.truncate sample.project.project_name ]
--        ]
--
--
--nameColumn : Table.Column { a | sample_id : Int, sample_name : String } Msg
--nameColumn =
--    Table.veryCustomColumn
--        { name = "Sample"
--        , viewData = nameLink
--        , sorter = Table.increasingOrDecreasingBy (String.toLower << .sample_name)
--        }
--
--
--nameLink : { a | sample_id : Int, sample_name : String } -> Table.HtmlDetails Msg
--nameLink sample =
--    Table.HtmlDetails []
--        [ a [ Route.href (Route.Sample sample.sample_id) ]
--            [ text <| Util.truncate sample.sample_name ]
--        ]
--
--
--removeFromCartColumn : Table.Column { a | sample_id : Int, sample_name : String } Msg
--removeFromCartColumn =
--    Table.veryCustomColumn
--        { name = ""
--        , viewData = removeFromCartLink
--        , sorter = Table.unsortable
--        }
--
--
--removeFromCartLink : { a | sample_id : Int, sample_name : String } -> Table.HtmlDetails Msg
--removeFromCartLink sample =
--    Table.HtmlDetails []
--        [ removeFromCartButton sample.sample_id |> Html.map (\_ -> RemoveFromCart sample.sample_id)
--        ]


--removeFromCartButton : Int -> Html Msg
--removeFromCartButton id =
--    button [ class "btn btn-default btn-xs", onClick (RemoveFromCart id) ] [ text "Remove" ]


addToCartButton : Cart -> List Int -> Html Msg
addToCartButton cart idList =
    let
        btn label clickMsg =
            button [ class "btn btn-xs btn-outline-secondary", onClick clickMsg ]
                [ text label ]
    in
    if List.any (\id -> contains cart id) idList then
        btn "Remove" (RemoveFromCart idList)
    else
        btn "Add" (AddToCart idList)


--FIXME merge with addToCartButton
addToCartButton2 : Cart -> List Int -> Html Msg
addToCartButton2 cart idList =
    let
        btn label clickMsg =
            button [ class "btn btn-sm btn-outline-secondary", onClick clickMsg ]
                [ Icon.shoppingCart
                , text " "
                , text label
                ]
    in
    if List.any (\id -> contains cart id) idList then
        btn "Remove from Cart" (RemoveFromCart idList)
    else
        btn "Add to Cart" (AddToCart idList)


addAllToCartButton : Cart -> Maybe (String, String) -> List Int -> Html Msg
addAllToCartButton (Cart cart) optionalLabels idList =
    let
        (addLbl, removeLbl) =
            case optionalLabels of
                Just labels ->
                    labels

                Nothing ->
                    ( "Add All", "Remove All" )

        intersection =
            Set.intersect (Set.fromList idList) cart.contents |> Set.toList

        btn label clickMsg =
            button [ class "btn btn-xs btn-outline-secondary align-middle", onClick clickMsg ]
                [ Icon.shoppingCart
                , text " "
                , text label
                ]
    in
    if intersection == [] then
        btn addLbl (AddToCart idList)
    else
        btn removeLbl (RemoveFromCart idList)


--samplesInCart : Cart -> List { a | id : Int } -> List { a | id : Int }
--samplesInCart (Cart cart) samples =
--    List.filter (\sample -> Set.member sample.id cart.contents) samples


--size : Model -> Int
--size model =
--    Set.size model.cart.contents
