{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Graphics.Svg
import qualified Parser
import Visual

main :: IO ()
main = do
    let (Right visual) = Parser.parse "bitraverse :: (Bitraversable t, Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)"
        container = [Version_ <<- "1.1", Width_ <<- "2500", Height_ <<- "500"]
        blobble = Blobble{x = 5, y = 5, w = 2000, r = 50}
        svg = doctype <> with (svg11_ (renderSvg blobble visual)) container
    renderToFile "index.html" svg