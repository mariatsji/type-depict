module Visual where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Foldable (fold)
import Graphics.Svg

data Visual
    = Fix Visual
    | Connect [Visual]
    | Embellish Visual
    | Group Visual
    | Dot String
    deriving stock (Eq)

render :: Visual -> Text
render = \case
    Dot _ -> "."
    Connect xs -> T.intercalate "--" $ fmap render xs
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
    Connect [a,b] -> 
        let bubbleB = jump bubble
            path = path_ [D_ <<- rightEdge mA bubble <> leftEdge lA bubbleB, Stroke_ <<- "black", Stroke_width_ <<- "3"]
        in renderSvg bubble a <> path <> renderSvg bubbleB b
    Connect xs ->
        foldMap
            ( \visual ->
                let bubbleA = jump bubble
                    path = path_ [D_ <<- rightEdge mA bubble <> leftEdge lA bubbleA, Stroke_ <<- "black", Stroke_width_ <<- "3"]
                 in renderSvg bubble visual <> path
            )
            xs
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

    jump :: Bubble -> Bubble
    jump Bubble{..} = Bubble{cx = cx + (3 * r), cy = cy, r = r}