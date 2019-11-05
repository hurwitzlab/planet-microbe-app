module BarChart exposing (Config, defaultConfig, view)

-- Modified from https://code.gampleman.eu/elm-visualization/BarChart/
--TODO move into own elm package

import Axis
import Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import TypedSvg exposing (g, rect, style, svg, text_, tspan)
import TypedSvg.Attributes exposing (class, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Transform(..), Length(..))


type alias Config a =
    { width : Float
    , height : Float
    , padding : Float
    , formatter : (a -> String)
    , title : Maybe String
    , maxLabelLen : Int
    }


defaultConfig : Config String
defaultConfig =
    { width = 900
    , height = 450
    , padding = 30
    , formatter = (\s -> s)
    , title = Nothing
    , maxLabelLen = 25
    }


titleHeight : Float
titleHeight
    = 17


xScale : Float -> Float -> List ( a, Float ) -> BandScale a
xScale w padding model =
    List.map Tuple.first model
        |> Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } ( 0, w - 2 * padding )


yScale : Float -> Float -> (Float, Float) -> ContinuousScale Float
yScale h padding range =
    Scale.linear ( h - 2 * padding, 0 ) range


xAxis : Float -> Float -> Int -> List ( a, Float ) -> (a -> String) -> Svg msg
xAxis w padding maxLabelLen model formatter =
    Axis.bottom [] (Scale.toRenderable (formatter >> truncate maxLabelLen) (xScale w padding model))


truncate : Int -> String -> String
truncate len s =
    if String.length s > len then
        (String.slice 0 (len - 3) s) ++ "..."
    else
        s


yAxis : Float -> Float -> (Float, Float) -> Svg msg
yAxis h padding range =
    Axis.left [ Axis.tickCount 5 ] (yScale h padding range)


column : Float -> Float -> Float -> (Float, Float) -> (a -> String) -> BandScale a -> (a, Float ) -> Svg msg
column w h padding range formatter scale ( label, value ) =
    g [ class [ "column" ] ]
        [ rect
            [ x <| Scale.convert scale label
            , y <| Scale.convert (yScale h padding range) value
            , width <| Scale.bandwidth scale
            , height <| h - Scale.convert (yScale h padding range) value - 2 * padding
            ]
            []
        , text_
            [ x <| Scale.convert (Scale.toRenderable formatter scale) label
            , y <| Scale.convert (yScale h padding range) value - 2
            , textAnchor AnchorMiddle
            ]
            [ text <| (String.fromFloat value) ]
        ]


view : Config a -> List ( a, Float ) -> Svg msg
view config model =
    let
        min =
            model |> List.map Tuple.second |> List.minimum |> Maybe.withDefault 0

        max =
            model |> List.map Tuple.second |> List.maximum |> Maybe.withDefault 10

        range =
            ( if min < 0 then min else 0 -- Always start from 0 on y axis unless min is negative
            , max
            )

        maxLabelLen =
            model |> List.map (Tuple.first >> config.formatter >> truncate config.maxLabelLen >> String.length) |> List.maximum |> Maybe.withDefault 0

        padding =
            config.padding

        chartWidth =
            config.width - (toFloat maxLabelLen) * 1.5 -- account for x-axis labels width

        chartHeight =
            config.height - 2 * config.padding - (toFloat maxLabelLen) * 1.5 -- account for x-axis labels height
    in
    svg [ width config.width, height config.height] --[ viewBox 0 0 config.width config.height ]
        [ style [] [ text """
            .column rect { fill: #00bbcc; }
            .column text { display: inline; font-size: 0.65em; }
            .xaxis .tick text { writing-mode: tb; text-anchor: start; transform:translate(-12px,0px) rotate(-45deg);" }
          """ ]
        , case config.title of
            Just title ->
                text_ [ textAnchor AnchorMiddle, TypedSvg.Attributes.x (Percent 50), y (config.padding - titleHeight) ] [ text title ]

            Nothing ->
                g [] []
        , g [ transform [ Translate (padding - 1) (chartHeight - padding) ], class [ "xaxis" ] ]
            [ xAxis chartWidth padding config.maxLabelLen model config.formatter ]
        , g [ transform [ Translate padding padding ] ]
            [ yAxis chartHeight padding range ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ] <|
            List.map (column chartWidth chartHeight padding range config.formatter (xScale chartWidth padding model)) model
        ]