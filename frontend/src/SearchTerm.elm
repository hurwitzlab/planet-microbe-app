module SearchTerm exposing (SearchTerm, PURL, Annotation, Value(..), Distribution, searchTermDecoder, valueDecoder, distributionDecoder, viewValue)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)
import Dict exposing (Dict)
import Html exposing (Html, a, text)
import Html.Attributes exposing (href, target)



-- TYPES --


type alias SearchTerm =
    { type_ : String
    , id : PURL
    , label : String
    , definition : String
    , unitId : PURL
    , unitLabel : String
    , sourceUrl : String
    , alias_ : String
    , aliases : List Alias
    , annotations : List Annotation
    , distribution : Distribution
    -- Only for type "number"
    , min : Float
    , max : Float
    -- Only for type "string"
    , purlLabels : Dict String String
    }


type alias PURL =
    String


type alias Alias =
    { name : String
    , sourceName : String
    , sourceUrl : String
    }


type alias Annotation =
    { id : String
    , label : String
    , value : String
    }


type Value
    = StringValue String
    | IntValue Int
    | FloatValue Float


type alias Distribution =
    List (String, Int)



-- SERIALIZATION --


searchTermDecoder : Decoder SearchTerm
searchTermDecoder =
    Decode.succeed SearchTerm
        |> required "type" Decode.string
        |> required "id" Decode.string
        |> required "label" Decode.string
        |> optional "definition" Decode.string ""
        |> optional "unitId" Decode.string ""
        |> optional "unitLabel" Decode.string ""
        |> optional "sourceUrl" Decode.string ""
        |> optional "alias" Decode.string ""
        |> optional "aliases" (Decode.list aliasDecoder) []
        |> optional "annotations" (Decode.list annotationDecoder) []
        |> optional "distribution" distributionDecoder []
        |> optional "min" Decode.float 0
        |> optional "max" Decode.float 0
        |> optional "purlLabels" (Decode.dict Decode.string) Dict.empty


aliasDecoder : Decoder Alias
aliasDecoder =
    Decode.succeed Alias
        |> required "name" Decode.string
        |> required "sourceName" Decode.string
        |> required "sourceUrl" Decode.string


annotationDecoder : Decoder Annotation
annotationDecoder =
    Decode.succeed Annotation
        |> required "id" Decode.string
        |> required "label" Decode.string
        |> required "value" Decode.string


valueDecoder : Decoder (Maybe Value)
valueDecoder =
    Decode.nullable
        (Decode.oneOf
            [ Decode.map StringValue Decode.string
            , Decode.map IntValue Decode.int
            , Decode.map FloatValue Decode.float
            ]
        )


distributionDecoder : Decoder (List (String, Int))
distributionDecoder =
    Decode.list (Decode.map2 Tuple.pair (Decode.index 0 Decode.string) (Decode.index 1 Decode.int))



-- VIEWS --

viewValue : String -> Html msg
viewValue val =
    if String.startsWith "http://" val || String.startsWith "https://" val || String.startsWith "ftp://" val then
        a [ href val, target "_blank" ] [ text val ]
    else
        text val

