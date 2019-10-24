module HistogramChart exposing (Config, defaultConfig, view)

-- Modified from https://code.gampleman.eu/elm-visualization/HistogramChart/
--TODO move into own elm package

import Axis
import Color
import Histogram exposing (Bin, HistogramGenerator)
import Scale exposing (BandConfig, BandScale, ContinuousScale)
import TypedSvg exposing (g, rect, svg)
import TypedSvg.Attributes exposing (class, fill, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Transform(..))


type alias Config =
    { width : Float
    , height : Float
    , padding : Float
    , domain : ( Float, Float )
    }


defaultConfig : Config
defaultConfig =
    { width = 900
    , height = 450
    , padding = 30
    , domain = ( 0, 20 )
    }


histogram : ( Float, Float ) -> List Float -> List (Bin Float Float)
histogram domain model =
    Histogram.float
        |> Histogram.withDomain domain
        |> Histogram.compute model


xScale : Float -> Float -> (Float, Float) -> ContinuousScale Float
xScale width padding domain =
    Scale.linear ( 0, width - 2 * padding ) domain


yScaleFromBins : Float -> Float -> List (Bin Float Float) -> ContinuousScale Float
yScaleFromBins height padding bins =
    List.map .length bins
        |> List.maximum
        |> Maybe.withDefault 0
        |> toFloat
        |> Tuple.pair 0
        |> Scale.linear ( height - 2 * padding, 0 )


xAxis : Float -> Float -> (Float, Float) -> List Float -> Svg msg
xAxis width padding domain model =
    Axis.bottom [] (xScale width padding domain)


yAxis : Float -> Float -> List (Bin Float Float) -> Svg msg
yAxis height padding bins =
    Axis.left [ Axis.tickCount 5 ] (yScaleFromBins height padding bins)


column : Float -> Float -> Float -> (Float, Float) -> ContinuousScale Float -> Bin Float Float -> Svg msg
column w h padding domain yScale { length, x0, x1 } =
    rect
        [ x <| Scale.convert (xScale w padding domain) x0
        , y <| Scale.convert yScale (toFloat length)
        , width <| Scale.convert (xScale w padding domain) x1 - Scale.convert (xScale w padding domain) x0
        , height <| h - Scale.convert yScale (toFloat length) - 2 * padding
        , fill <| Fill <| Color.rgb255 46 118 149
        ]
        []


view : Config -> List Float -> Svg msg
view config model =
    let
        bins =
            histogram config.domain model
    in
    svg [ viewBox 0 0 config.width config.height  ]
        [ g [ transform [ Translate (config.padding - 1) (config.height - config.padding) ] ]
            [ xAxis config.width config.padding config.domain model ]
        , g [ transform [ Translate (config.padding - 1) config.padding ] ]
            [ yAxis config.height config.padding bins ]
        , g [ transform [ Translate config.padding config.padding ], class [ "series" ] ] <|
            List.map (column config.width config.height config.padding config.domain (yScaleFromBins config.height config.padding bins)) bins
        ]
