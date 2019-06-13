port module GAnalytics exposing (send)

import Json.Encode as Encode exposing (Value)



send : String -> String -> Cmd msg
send page trackingId =
    updateAnalytics (encode page trackingId)


port updateAnalytics : Value -> Cmd msg


encode : String -> String -> Value
encode page trackingId =
    Encode.object
        [ ( "page", Encode.string page )
        , ( "trackingId", Encode.string trackingId )
        ]