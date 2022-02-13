{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import qualified Buffer
import Parser

import Test.Hspec

spec :: Spec
spec = describe "Parser" $ do
    it "should be alive" $ do
        'b' `shouldBe` 'b'
    {- it "should Parse a single word as a buffer with the word pointing to []" $ do
        Buffer.get "hello" <$> Parser.parse "hello"
        `shouldBe`
        Right []
 -}