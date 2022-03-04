{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Graphics.Svg
import qualified Parser
import Visual

main :: IO ()
main = do
    let visual = Connect [Group (Dot "a"), Group (Dot "a"), Group (Dot "a")]
        container = [Version_ <<- "1.1", Width_ <<- "2500", Height_ <<- "500"]
        blobble = Blobble{x = 5, y = 5, w = 2000, r = 150}
        svg = doctype <> with (svg11_ (renderSvg blobble visual)) container
    renderToFile "index.html" svg