{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Graphics.Svg
import qualified Parser
import Visual

main :: IO ()
main = do
    let (Right visual) = Parser.parse "(a -> m b ) -> m a -> m b"
        container = [Version_ <<- "1.1", Width_ <<- "3000", Height_ <<- "300"]
        bubble = Bubble{cx = 150, cy = 150, r = 150}
        svg = doctype <> with (svg11_ (renderSvg bubble visual)) container
    renderToFile "index.html" svg