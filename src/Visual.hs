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

data Bubble = Bubble
    { cx :: Float -- how far along the total axis is this bubble
    , cy :: Float -- how high up is this bubble bubble
    , r :: Float -- radius of this bubble
    }

renderSvg :: Bubble -> Visual -> Element
renderSvg bubble@Bubble{..} = \case
    Dot _ ->
        let cT = T.pack . show
         in circle_ [Cx_ <<- cT cx, Cy_ <<- cT cy, R_ <<- "4", Fill_ <<- "black"]
    Connect a b ->
        let (bubbleA, bubbleB) = split bubble
            path = path_ [D_ <<- rightEdge mA bubbleA <> leftEdge lA bubbleB, Stroke_ <<- "black", Stroke_width_ <<- "3"]
         in renderSvg bubbleA a <> path <> renderSvg bubbleB b
    Embellish a ->
        let bubbleA = Bubble{cx = cx, cy = cy, r = r - 10}
            cT = T.pack . show
         in circle_ [Cx_ <<- cT cx, Cy_ <<- cT cy, R_ <<- T.pack (show r), Stroke_ <<- "black", Stroke_width_ <<- "3", Fill_ <<- "none"] <> renderSvg bubbleA a
    Group a ->
        let bubbleA = Bubble{cx = cx, cy = cy, r = r - 10}
            cT = T.pack . show
         in circle_ [Cx_ <<- cT cx, Cy_ <<- cT cy, R_ <<- T.pack (show r), Stroke_ <<- "black", Stroke_dasharray_ <<- "4", Stroke_width_ <<- "3", Fill_ <<- "none"] <> renderSvg bubbleA a
    Fix a ->
        let bubbleA = Bubble{cx = cx, cy = cy, r = r - 10}
            cT = T.pack . show
         in circle_ [Cx_ <<- cT cx, Cy_ <<- cT cy, R_ <<- T.pack (show r), Stroke_ <<- "black", Stroke_width_ <<- "3", Fill_ <<- "none"]
         <> path_ [D_ <<- bottomEdge mA bubble <> lR (-10) 10, Stroke_ <<- "black", Stroke_width_ <<- "3"]
         <> path_ [D_ <<- bottomEdge mA bubble <> lR (-10) (-10), Stroke_ <<- "black", Stroke_width_ <<- "3"]
         <> renderSvg bubbleA a
  where
    rightEdge :: (Float -> Float -> Text) -> Bubble -> Text
    rightEdge svgOp Bubble{..} = svgOp (cx + r) cy

    leftEdge :: (Float -> Float -> Text) -> Bubble -> Text
    leftEdge svgOp Bubble{..} = svgOp (cx - r) cy

    bottomEdge :: (Float -> Float -> Text) -> Bubble -> Text
    bottomEdge svgOp Bubble{..} = svgOp cx (cy + r) 

    split :: Bubble -> (Bubble, Bubble)
    split Bubble{..} =
        let cxA = cx - r + (r / 3)
            cxB = cx + r - (r / 3)
         in (Bubble{cx = cxA, cy = cy, r = r / 3}, Bubble{cx = cxB, cy = cy, r = r / 3})