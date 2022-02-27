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
            cxT = T.pack . show $ cx
            cy = y + h / 2
            cyT = T.pack . show $ cy
         in circle_ [Cx_ <<- cxT, Cy_ <<- cyT, R_ <<- "4", Fill_ <<- "black"]
    Connect a b ->
        let boxA = Box{w = w / 2, h = h, x = x, y = y}
            boxB = Box{w = w / 2, h = h, x = w / 2, y = y}
            path = path_ [D_ <<- centre mA boxA <> centre lA boxB, Stroke_ <<- "black"]
         in renderSvg boxA a <> path <> renderSvg boxB b
  where
    centre :: (Float -> Float -> Text) -> Box -> Text
    centre svgOp Box{..} = svgOp (x + (w / 2)) (y + (h / 2))

