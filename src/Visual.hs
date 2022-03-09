module Visual where

import Data.List (uncons)
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
    deriving (Show)

renderSvg :: Blobble -> Visual -> Element
renderSvg blobble@Blobble{..} = \case
    Dot _ ->
        let mid = x + r + w / 2
         in circle_ [Cx_ <<- cT mid, Cy_ <<- cT (y + r), R_ <<- "5", Fill_ <<- "black"]
    Embellish a ->
        let rect = rect_ [X_ <<- cT x, Y_ <<- cT y, Width_ <<- cT (r + r + w), Height_ <<- cT (2 * r), Rx_ <<- cT r, Fill_ <<- "none", Stroke_ <<- "black", Stroke_width_ <<- "3"]
         in rect <> renderSvg (shrink blobble) a
    Group a ->
        let rect = rect_ [X_ <<- cT x, Y_ <<- cT y, Width_ <<- cT (r + r + w), Height_ <<- cT (2 * r), Rx_ <<- cT r, Fill_ <<- "none", Stroke_ <<- "black", Stroke_width_ <<- "3", Stroke_dasharray_ <<- "4"]
         in rect <> renderSvg (shrink blobble) a
    Fix a ->
        renderSvg blobble (Embellish a)
            <> path_ [D_ <<- mA (x + r + w / 2 + 20) (y + 2 * r) <> lR (-20) 20, Stroke_ <<- "black", Stroke_width_ <<- "3"]
            <> path_ [D_ <<- mA (x + r + w / 2 + 20) (y + 2 * r) <> lR (-20) (-20), Stroke_ <<- "black", Stroke_width_ <<- "3"]
            <> renderSvg (shrink blobble) a
    Connect xs ->
        let blobbles = split (length xs) blobble
            zipped = zip blobbles xs
         in foldMap
                (uncurry renderSvg)
                zipped
            <> connectLines zipped

split :: Int -> Blobble -> [Blobble]
split n parent =
    if n < 2 then [parent]
    else
        fmap (foo parent n) [ 1 .. n ]
    where
        foo :: Blobble -> Int -> Int -> Blobble
        foo Blobble{..} total idx =
            let r' = r
                c = if idx == 1 then 0 else r'
                w' = (w + 3 * r - (3 * fromIntegral total * r)) / 2
            in Blobble {
                x = x + ((r' + w' + r' + c) * fromIntegral (pred idx)),
                y = y,
                r = r,
                w = w'
            }




split' :: Int -> Blobble -> [Blobble]
split' parts super@Blobble{..} =
    if parts < 1
        then []
        else
            let w' = ( (r + r + w ) / fromIntegral parts - 4 * r)
             in mkBlobble super w' <$> [0 .. pred parts]
  where
    mkBlobble :: Blobble -> Float -> Int -> Blobble
    mkBlobble Blobble{..} myW i =
        Blobble
            { r = r
            , x = x + fromIntegral i * ( myW + 4 * r )
            , w = myW
            , y = y
            }

connectLines :: [(Blobble, Visual)] -> Element
connectLines [b1, b2] = path_ [D_ <<- rightEdge mA b1 <> leftEdge lA b2, Stroke_ <<- "black", Stroke_width_ <<- "4"]
connectLines (b1 : xs) =
    case uncons xs of
        Nothing -> mempty
        Just (t, xs') ->  connectLines [b1, t] <> connectLines ( t : xs' )
connectLines _ = mempty

rightEdge :: (Float -> Float -> Text) -> (Blobble, Visual) -> Text
rightEdge svgOp (Blobble{..}, Dot _) = svgOp (x + r + ( w / 2 )) (y + r)
rightEdge svgOp (Blobble{..}, _) = svgOp (x + w + ( 2 * r )) (y + r)

leftEdge :: (Float -> Float -> Text) -> (Blobble, Visual) -> Text
leftEdge svgOp (Blobble{..}, Dot _) = svgOp (x + r + ( w / 2 ) ) (y + r)
leftEdge svgOp (Blobble{..}, _) = svgOp x (y + r)

cT :: Float -> Text
cT = T.pack . show

shrink :: Blobble -> Blobble
shrink Blobble{..} = Blobble{x = x + 8, y = y + 8, w = w - 1, r = r - 8}

