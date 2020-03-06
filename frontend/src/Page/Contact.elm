module Page.Contact exposing (Model, Msg, init, toSession, update, view)

import Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Page
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Config exposing (apiBaseUrl)



---- MODEL ----


type alias Model =
    { session : Session
    , name : String
    , email : String
    , message : String
    , sent : Bool
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        user =
            Session.getUser session

        email =
            user |> Maybe.map .email |> Maybe.withDefault ""

        name =
            user
                |> Maybe.map (\u -> u.first_name ++ " " ++ u.last_name)
                |> Maybe.withDefault ""
    in
    (
        { session = session
        , name = name
        , email = email
        , message = ""
        , sent = False
        }
    , Cmd.none
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE --


type Msg
    = SetName String
    | SetEmail String
    | SetMessage String
    | Submit
    | SubmitDone (Result Http.Error ContactResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetName name ->
            ( { model | name = name }, Cmd.none )

        SetEmail email ->
            ( { model | email = email }, Cmd.none )

        SetMessage message ->
            ( { model | message = message }, Cmd.none )

        Submit ->
            ( { model | sent = True }, submit model )

        SubmitDone response ->
            ( { model | sent = True }, Cmd.none )


submit : Model -> Cmd Msg
submit model =
    let
        url =
            apiBaseUrl ++ "/contact"
    in
    Http.send SubmitDone
        (Http.post url (encodeContact model) contactDecoder)


encodeContact : Model -> Http.Body
encodeContact model =
    Http.jsonBody <|
        Encode.object
            [ ("name", Encode.string model.name)
            , ("email", Encode.string model.email)
            , ("message", Encode.string model.message)
            ]


type alias ContactResponse =
    { status : String
    }


contactDecoder : Decoder ContactResponse
contactDecoder =
    Decode.succeed ContactResponse
        |> Pipeline.required "status" Decode.string



-- VIEW --


view : Model -> Html Msg
view model =
    if model.sent then
        div [ class "container" ]
            [ div [ class "jumbotron", style "margin-top" "10vh" ]
                [ h3 [] [ text "Message sent." ]
                , p [] [ text "We will respond to your message as soon as possible." ]
                , p [] [ text "Thanks for your feedback and for using Planet Microbe!" ]
                ]
            ]
    else
        div [ class "container" ]
            [ div [ class "row" ]
                [ div [ class "col pt-5" ]
                    [ Page.viewTitle1 "Contact Us" True
                    ]
                ]
            , div [ class "row" ]
                [ div [ class "col" ]
                    [ div [ class "pt-4" ]
                        [ p []
                            [ text "Please complete the form below to send us your bug reports, comments, and questions." ]
                        , p [ class "pt-1" ]
                            [ text "If you are looking for documentation, see the "
                            , a [ href "https://hurwitzlab.gitbook.io/planet-microbe-documentation/", target "_blank" ] [ text "User Manual" ]
                            , text "."
                            ]
                        ]
                    , div [ class "pt-4 w-50" ]
                        [ div [ class "form-group" ]
                            [ label [ attribute "for" "name" ] [ text "Your name" ]
                            , input [ type_ "text", class "form-control", placeholder "Enter your name", value model.name, onInput SetName ] []
                            ]
                        , div [ class "form-group" ]
                            [ label [ attribute "for" "email" ] [ text "Your email" ]
                            , input [ type_ "email", class "form-control", placeholder "Enter your email", value model.email, onInput SetEmail ] []
                            ]
                        , div [ class "form-group" ]
                            [ label [ attribute "for" "message" ] [ text "Your message" ]
                            , textarea [ class "form-control", rows 6, onInput SetMessage ] []
                            ]
                        , button [ class "btn btn-primary", onClick Submit ] [ text "Submit" ]
                        ]
                    ]
                ]
            ]