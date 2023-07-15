module PieChart exposing ( update)


import Array exposing (Array)
import Color exposing (Color)
import Path
import Shape exposing (defaultPieConfig)
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (dy, fill, stroke, textAnchor, transform, viewBox)
--import TypedSvg.Attributes.InPx exposing (height, width)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..), Transform(..), em)
import Login exposing (Msg)

w : Float
w =
    330

h : Float
h =
    100

colors : Array Color
colors =
    Array.fromList
        [ Color.rgb255 152 171 198
        , Color.rgb255 138 137 166
        , Color.rgb255 123 104 136
        ]

radius : Float
radius =
    min w h / 2

pieSlice : Int -> Shape.Arc -> Svg msg
pieSlice index datum =
    Path.element (Shape.arc datum) [ fill <| Paint <| Maybe.withDefault Color.black <| Array.get index colors, stroke <| Paint Color.white ]

pieLabel : Shape.Arc -> ( String, Float ) -> Svg msg
pieLabel slice ( label, _ ) =
    let
        ( x, y ) =
            Shape.centroid { slice | innerRadius = radius - 15, outerRadius = radius - 20 }
    in
    text_
        [ transform [ Translate x y ]
        , dy (em 0.05)
        , textAnchor AnchorMiddle
        ]
        [ text label ]

view : List ( String, Float ) -> Svg msg
view model =
    let
        pieData =
            model |> List.map Tuple.second |> Shape.pie { defaultPieConfig | outerRadius = radius }
    in
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (w / 2) (h / 2) ] ]
            [ g [] <| List.indexedMap pieSlice pieData
            , g [] <| List.map2 pieLabel pieData model
            ]
        ]

update : List ( String, Float ) -> Svg msg
update pieData = 
    view pieData