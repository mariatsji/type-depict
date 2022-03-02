module Visual where

import Data.Text (Text)
import qualified Data.Text as T

import Graphics.Svg

data Visual
    = Fix Visual
    | Connect Visual Visual
    | Embellish Visual
    | Group Visual
    | Dot String
    deriving stock (Eq)

render :: Visual -> Text
render = \case
    Dot _ -> "."
    Connect a b -> render a <> "--" <> render b
    Embellish a -> "(" <> render a <> ")"
    Fix a -> "@" <> render a
    Group a -> "{" <> render a <> "}"

instance Show Visual where
    show = T.unpack . render

data Box = Box
    { w :: Float -- my width
    , h :: Float -- my height
    , x :: Float -- offset x in container
    , y :: Float -- offset y in container
    }

renderSvg :: Box -> Visual -> Element
renderSvg box@Box{..} = \case
    Dot _ ->
        let cx = x + w / 2
            cy = y + h / 2
            cT = T.pack . show
         in circle_ [Cx_ <<- cT cx, Cy_ <<- cT cy, R_ <<- "4", Fill_ <<- "black"]
    Connect a b ->
        let boxA = Box{w = w / 2, h = h, x = x, y = y}
            boxB = Box{w = w / 2, h = h, x = w / 2, y = y}
            path = path_ [D_ <<- centre mA boxA <> centre lA boxB, Stroke_ <<- "black"]
         in renderSvg boxA a <> path <> renderSvg boxB b
    Embellish a ->
        let boxA = Box{w = w - 50, h = h - 50, x = x + 25, y = y + 25}
            cx = x + w / 2
            cy = y + h / 2
            cT = T.pack . show
        in circle_ [Cx_ <<- cT cx, Cy_ <<- cT cy, R_ <<- "100", Stroke_ <<- "black", Stroke_width_ <<- "3", Fill_ <<- "none"] <> renderSvg boxA a
  where
    centre :: (Float -> Float -> Text) -> Box -> Text
    centre svgOp Box{..} = svgOp (x + (w / 2)) (y + (h / 2))

