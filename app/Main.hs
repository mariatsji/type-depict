{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Graphics.Svg
import Visual

main :: IO ()
main = do
    let visual = Connect (Dot "a") (Embellish (Dot "b"))
        container = [Version_ <<- "1.1", Width_ <<- "600", Height_ <<- "300"]
        box = Box{w = 600, h = 300, x = 0, y = 0}
        svg = doctype <> with (svg11_ (renderSvg box visual)) container
    renderToFile "index.html" svg