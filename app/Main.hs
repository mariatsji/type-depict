{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Graphics.Svg
import Visual

main :: IO ()
main = do
    let visual = Embellish $ Connect (Embellish (Dot "a")) (Embellish (Dot "b"))
        container = [Version_ <<- "1.1", Width_ <<- "600", Height_ <<- "300"]
        bubble = Bubble{cx = 300, cy = 150, r = 150}
        svg = doctype <> with (svg11_ (renderSvg bubble visual)) container
    renderToFile "index.html" svg