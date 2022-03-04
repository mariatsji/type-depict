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

data Blobble = Blobble
    { w :: Float -- the width (without 2r)
    , r :: Float -- radius of this bubble
    , x :: Float -- absolute offset x in a larger Blobble
    , y :: Float -- absolute offset y in a larger Blobble
    }

renderSvg :: Blobble -> Visual -> Element
renderSvg blobble@Blobble{..} = \case
    Dot _ ->
        let mid = x + r + w / 2
         in circle_ [Cx_ <<- cT mid, Cy_ <<- cT (y + r), R_ <<- "5", Fill_ <<- "black"]
    Embellish a ->
        let rect = rect_ [X_ <<- cT x, Y_ <<- cT y, Width_ <<- cT (2 * r + w), Height_ <<- cT (2 * r), Rx_ <<- cT r, Fill_ <<- "none", Stroke_ <<- "black", Stroke_width_ <<- "3"]
         in rect <> renderSvg (shrink blobble) a
    Group a ->
        let rect = rect_ [X_ <<- cT x, Y_ <<- cT y, Width_ <<- cT (2 * r + w), Height_ <<- cT (2 * r), Rx_ <<- cT r, Fill_ <<- "none", Stroke_ <<- "black", Stroke_width_ <<- "3", Stroke_dasharray_ <<- "4"]
         in rect <> renderSvg (shrink blobble) a
    Fix a -> 
        renderSvg blobble (Embellish a)
        <> path_ [D_ <<- mA (x + r + w / 2) (y + 2 * r) <> lR (-20) 20, Stroke_ <<- "black", Stroke_width_ <<- "4"]
        <> path_ [D_ <<- mA (x + r + w / 2) (y + 2 * r) <> lR (-20) (-20), Stroke_ <<- "black", Stroke_width_ <<- "4"]
        <> renderSvg (shrink blobble) a
    Connect xs ->
        let blobbles = split (length xs) blobble
        

cT :: Float -> Text
cT = T.pack . show

shrink :: Blobble -> Blobble
shrink Blobble{..} = Blobble{x = x + 8, y = y + 8, w = w - 1, r = r - 8}

split :: Int -> Blobble -> [Blobble]
split parts super@Blobble{..} =
    let newWidth = w / (fromIntegral parts) - 2 * r
     in mkBlobble super newWidth <$> [0 .. parts]
  where
    mkBlobble :: Blobble -> Float -> Int -> Blobble
    mkBlobble Blobble{..} width i =
        Blobble
            { r = r
            , x = x + (fromIntegral i * r)
            , w = width
            , y = y
            }

{-- Connect xs ->
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

rightEdge :: (Float -> Float -> Text) -> Bubble -> Text
rightEdge svgOp Bubble{..} = svgOp (cx + r) cy

leftEdge :: (Float -> Float -> Text) -> Bubble -> Text
leftEdge svgOp Bubble{..} = svgOp (cx - r) cy

bottom :: (Float -> Float -> Text) -> Bubble -> Text
bottom svgOp Bubble{..} = svgOp ( r + w / 2 ) r

jump :: Bubble -> Bubble
jump Bubble{..} = Bubble{cx = cx + (3 * r), cy = cy, r = r} -}

