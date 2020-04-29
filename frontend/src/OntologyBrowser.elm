module OntologyBrowser exposing (Model, Msg(..), init, update, insert, view)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (type_, class, style, placeholder, value)
import Html.Events exposing (onInput)
import Treeview
import Page



-- MODEL --


type alias Model =
    { tree : Treeview.Model
    , searchVal : String
    , status : Status
    }


type Status
    = LOADING
    | READY


init : Model
init =
    Model [] "" LOADING



-- UPDATE --


type Msg
    = NoOp
    | SetSearchVal String
    | Search
    | Selected String
    | FetchSubclasses String
    | TreeviewMsg Treeview.Msg


update : Msg -> Model -> ( Model, Msg )
update msg model =
    case msg of
        SetSearchVal s ->
            let
                ( newStatus, newMsg ) =
                    if s == "" || String.length s >= 4 then
                        ( LOADING, Search )
                    else
                        ( READY, NoOp )
            in
            ( { model | searchVal = s, status = newStatus }, newMsg )

        TreeviewMsg subMsg ->
            case subMsg of
                Treeview.Select id ->
                    let
                        checked =
                            case find id model.tree of
                                Just (Treeview.Node _ _ opt _) ->
                                    opt.checked

                                Nothing -> --impossible
                                    False

                        newTree =
                            Treeview.update (Treeview.ToggleCheck True True id checked) model.tree
                    in
                    ( { model | tree = newTree }, NoOp )

                Treeview.Toggle id ->
                    let
                        selectedNode =
                            find id model.tree

                        tree =
                            Treeview.toggle id model.tree
                    in
                    case selectedNode of
                        Just node ->
                            if Treeview.nodeChildren node == Just [] then -- load children
                                ( { model | tree = tree, status = LOADING }, FetchSubclasses id )
                            else -- already loaded
                                ( { model | tree = tree }, NoOp )

                        _ -> -- impossible
                            ( model, NoOp )

                Treeview.ToggleCheck _ _ id checked ->
                    let
                        newTree =
                            Treeview.update subMsg model.tree
                    in
                    ( { model | tree = newTree }, Selected id )

                _ ->
                    let
                        newTree =
                            Treeview.update subMsg model.tree
                    in
                    ( { model | tree = newTree }, NoOp )

        _ ->
            ( model, NoOp )


find : String -> Treeview.Model -> Maybe Treeview.Node
find parentId tree =
    tree
        |> List.filterMap
            (\n ->
                if Treeview.nodeKey n == parentId then
                    Just n
                else if Treeview.nodeChildren n == Nothing then
                    Nothing
                else
                    find parentId (Treeview.nodeChildren n |> Maybe.withDefault [])
            )
        |> List.head


insertNodes : String -> List Treeview.Node -> Treeview.Model -> Treeview.Model
insertNodes parentId nodes tree =
    tree |>
        List.map
            (\n ->
                let
                    (Treeview.Node key _ opt _)
                        = n

                    children =
                        if key == parentId then
                            Just (List.map (\(Treeview.Node k t _ c) -> Treeview.Node k t { opt | opened = False } c) nodes) -- kludgey way to ensure node is closed
                        else
                            Just (insertNodes parentId nodes (Treeview.nodeChildren n |> Maybe.withDefault []))
                in
                Treeview.setNodeChildren children n
            )


insert : String -> List (String, String) -> Model -> Model
insert parentId subClasses model =
    let
        nodes =
            subClasses
                |> List.map
                    (\(id,lbl) ->
                        Treeview.node id lbl "" True (Just [])
                    )

        tree =
            if parentId == "" || model.tree == [] then -- initialize empty tree
                List.map Treeview.toggleNode nodes -- force closed
            else
                insertNodes parentId nodes model.tree
    in
    { model | tree = tree, status = READY }



-- VIEW --


view : Model -> Html Msg
view model =
    div []
        [ if model.status == LOADING then
            Page.viewSpinnerOverlay "17em" False
          else
            text ""
        , input [ type_ "text", class "form-control", placeholder "Search ...", value model.searchVal, onInput SetSearchVal ] []
        , div [ class "mt-3 mx-3", style "overflow-y" "auto", style "min-height" "50vh", style "max-height" "50vh" ]
            [ Treeview.view treeConfig model.tree |> Html.map TreeviewMsg
            ]
        ]


treeConfig : Treeview.Config
treeConfig =
    let
        d =
            Treeview.default []
    in
    { d | checkbox = { enable = True, multiple = True, cascade = True} }