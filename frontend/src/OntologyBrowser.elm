module OntologyBrowser exposing (..)

import Html exposing (Html, div, input)
import Html.Attributes exposing (type_, class, style, placeholder, value)
import Html.Events exposing (onInput)
import Treeview
import Debug


type alias Model =
    { tree : Treeview.Model
    , searchVal : String
    }


type Msg
    = NoOp
    | SetSearchVal String
    | Search
    | FetchSubclasses String
    | TreeviewMsg Treeview.Msg


update : Msg -> Model -> ( Model, Msg )
update msg model =
    case msg of
        SetSearchVal s ->
            ( { model | searchVal = s }, Search )

        TreeviewMsg subMsg ->
            let
                _ = Debug.log "TreeviewMsg" (Debug.toString subMsg)
            in
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
                                ( { model | tree = tree }
                                , FetchSubclasses id
                                )
                            else -- already loaded
                                ( { model | tree = tree }, NoOp )

                        _ -> -- impossible
                            ( model, NoOp )
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
                            Just (List.map (\(Treeview.Node k t _ c) -> Treeview.Node k t { opt | opened = False } c) nodes)
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
                |> List.map (\(id,lbl) -> Treeview.node id lbl "" True (Just []))
    in
    if parentId == "" || model.tree == [] then -- initialize empty tree
        { model | tree = List.map Treeview.toggleNode nodes } -- force closed
    else
        { model | tree = insertNodes parentId nodes model.tree }


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", class "form-control", placeholder "Search ...", value model.searchVal, onInput SetSearchVal ] []
        , div [ class "mt-3 mx-3", style "overflow-y" "auto", style "max-height" "50vh" ]
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