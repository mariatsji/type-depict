{-# language OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO
import Data.Text (Text)

data Cmd

main :: IO ()
main = do
    print "h(askell) (sh)ell"

    cmd <- TIO.getLine
    -- _ <- run cmd
    print "todo"

run :: Cmd -> IO Text
run cmd = pure ""