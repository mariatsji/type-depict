{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.State.Lazy
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Graphics.Svg
import qualified Parser
import Visual

main :: IO ()
main = do
    let (Right visual) = Parser.parse "(a -> b -> c) -> (a -> b -> c)"
        container = [Version_ <<- "1.1", Width_ <<- "2500", Height_ <<- "500"]
        blobble = Blobble{x = 5, y = 5, w = 2000, r = 50}
        s = renderSvg blobble visual
        svg = evalState s initEnv
        res = doctype <> with (svg11_ svg) container
    renderToFile "index.html" res

favicon :: IO ()
favicon = do
    let (Right visual) = Parser.parse "( f a b -> f a b ) -> f a b"
        container = [Version_ <<- "1.1", Width_ <<- "100", Height_ <<- "100"]
        blobble = Blobble{x = 5, y = 5, w = 5, r = 30}
        s = renderSvg blobble visual
        svg = evalState s initEnv
        res = doctype <> with (svg11_ svg) container
    renderToFile "assets/favicon.svg" res