module Page.Maintenance exposing (view)

import Html exposing (Html, div, h1, p, text)
import Html.Attributes exposing (class, style)



-- VIEW --


view : Html msg
view =
    div [ class "container"]
        [ div [ class "row" ]
            [ div [ style "width" "100%", style "padding" "10vh", style "vertical-align" "middle", style "text-align" "center" ]
                [ div [ style "padding" "40px 15px" ]
                    [ h1 [] [ text "Temporarily down for maintenance" ]
                    , h1 [] [ text "We’ll be back soon!" ]
                    ]
                , div []
                    [ p []
                        [ text "Sorry for the inconvenience but we’re performing some maintenance at the moment. "
                        , text "We’ll be back online shortly!"
                        ]
                    , p []
                        [ text "— The Planet Microbe Team" ]
                    ]
                ]
            ]
        ]