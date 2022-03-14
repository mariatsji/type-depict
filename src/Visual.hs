{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Visual where

import Control.Monad.Trans.State.Lazy
import Data.Foldable (fold)
import Data.Functor (($>))
import Data.HashMap.Lazy
import Data.List (uncons)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Debug.Trace
import Graphics.Svg
import Numeric (showHex)

data Visual
    = Fix Visual
    | Connect [Visual]
    | Embellish Visual
    | Group Visual
    | Dot (NonEmpty String)
    deriving stock (Eq)

render :: Visual -> Text
render = \case
    Dot xs -> T.pack $ '.' <$ NE.toList xs
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

data Color = Color {_r :: Word8, _g :: Word8, _b :: Word8} deriving stock (Eq)

data Env = Env
    { colors :: HashMap String Color
    , idx :: Int
    }

initEnv :: Env
initEnv =
    Env
        { colors = mempty
        , idx = 0
        }

renderSvg :: Blobble -> Visual -> State Env Element
renderSvg blobble@Blobble{..} = \case
    Dot words -> do
        let ys = dotV words blobble
            zipped = NE.zip words ys
            elemStates =
                traverse
                    ( \(word, y') -> do
                        s@Env{..} <- get
                        let (newEnv, c) = case Data.HashMap.Lazy.lookup word colors of
                                Nothing -> (s{colors = Data.HashMap.Lazy.insert word c colors, idx = succ idx}, newColor !! idx)
                                Just c -> (s, c)
                            midX = x + r + w / 2
                            el = circle_ [Cx_ <<- cT midX, Cy_ <<- cT y', R_ <<- "5", Fill_ <<- hex c]
                        put newEnv $> el
                    )
                    zipped
        mconcat . NE.toList <$> elemStates
    Embellish a -> do
        let rect = rect_ [X_ <<- cT x, Y_ <<- cT y, Width_ <<- cT (r + r + w), Height_ <<- cT (2 * r), Rx_ <<- cT r, Fill_ <<- "none", Stroke_ <<- "black", Stroke_width_ <<- "3"]
        el <- renderSvg (shrink blobble) a
        pure $ rect <> el
    Group a -> do
        let rect = rect_ [X_ <<- cT x, Y_ <<- cT y, Width_ <<- cT (r + r + w), Height_ <<- cT (2 * r), Rx_ <<- cT r, Fill_ <<- "none", Stroke_ <<- "black", Stroke_width_ <<- "3", Stroke_dasharray_ <<- "4"]
        el <- renderSvg (shrink blobble) a
        pure $ rect <> el
    Fix a -> do
        let arr =
                path_ [D_ <<- mA (x + r + w / 2 + 20) (y + 2 * r) <> lR (-20) 20, Stroke_ <<- "black", Stroke_width_ <<- "3"]
                    <> path_ [D_ <<- mA (x + r + w / 2 + 20) (y + 2 * r) <> lR (-20) (-20), Stroke_ <<- "black", Stroke_width_ <<- "3"]
        el <- renderSvg blobble (Embellish a)
        el2 <- renderSvg (shrink blobble) a
        pure $ el <> el2 <> arr
    Connect xs -> do
        let blobbles = split (length xs) blobble
            zipped = zip blobbles xs
            lines = connectLines zipped
            res =
                traverse
                    (uncurry renderSvg)
                    zipped
        mconcat . (lines :) <$> res

dotV :: NonEmpty String -> Blobble -> NonEmpty Float -- get y coords
dotV dots blob = go blob <$> NE.zip (NE.fromList [1 .. length dots]) dots
  where
    go :: Blobble -> (Int, String) -> Float
    go Blobble{..} (i, _) =
        let op = if odd i then (+) else (-)
         in r + y `op` (fromIntegral i * 6)

split :: Int -> Blobble -> [Blobble]
split n parent =
    if n < 2
        then [parent]
        else fmap (go parent n) [1 .. n]
  where
    go :: Blobble -> Int -> Int -> Blobble
    go Blobble{..} total idx =
        let r' = r
            c = if idx == 1 then 0 else r'
            n = fromIntegral total
            w' = (w - 3 * n * r + 3 * r) / n
         in Blobble
                { x = x + ((r' + w' + r' + c) * fromIntegral (pred idx))
                , y = y
                , r = r
                , w = w'
                }

connectLines :: [(Blobble, Visual)] -> Element
connectLines [b1, b2] = path_ [D_ <<- rightEdge mA b1 <> leftEdge lA b2, Stroke_ <<- "black", Stroke_width_ <<- "4"]
connectLines (b1 : xs) =
    case uncons xs of
        Nothing -> mempty
        Just (t, xs') -> connectLines [b1, t] <> connectLines (t : xs')
connectLines _ = mempty

rightEdge :: (Float -> Float -> Text) -> (Blobble, Visual) -> Text
rightEdge svgOp (Blobble{..}, Dot _) = svgOp (x + r + (w / 2)) (y + r)
rightEdge svgOp (Blobble{..}, _) = svgOp (x + w + (2 * r)) (y + r)

leftEdge :: (Float -> Float -> Text) -> (Blobble, Visual) -> Text
leftEdge svgOp (Blobble{..}, Dot _) = svgOp (x + r + (w / 2)) (y + r)
leftEdge svgOp (Blobble{..}, _) = svgOp x (y + r)

cT :: Float -> Text
cT = T.pack . show

shrink :: Blobble -> Blobble
shrink Blobble{..} = Blobble{x = x + 8, y = y + 8, w = w - 1, r = r - 8}

hex :: Color -> Text
hex Color{..} = "#" <> foldMap showHex2 [_r, _g, _b]
  where
    showHex2 :: forall a. (Integral a, Show a) => a -> Text
    showHex2 a = T.pack $ if a < 17 then "0" <> showHex a "" else showHex a ""

newColor :: [Color]
newColor = cycle [Color 0 0 255, Color 255 0 0, Color 0 255 0, Color 255 255 0, Color 0 255 255, Color 255 0 255, Color 0 0 0]