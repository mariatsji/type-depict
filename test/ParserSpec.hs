{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import qualified Buffer
import Data.Either (isRight)
import Data.Text (Text)


import Test.Hspec

instance Show Buffer.Buffer where
    show b = "<Buffer>"

spec :: Spec
spec = describe "Parser" $ do
    it "should be alive" $ do
        'b' `shouldBe` 'b'
    