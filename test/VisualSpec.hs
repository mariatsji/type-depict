{-# language OverloadedStrings #-}

module VisualSpec where

import Visual

import Test.Hspec

spec :: Spec
spec = describe "Parser" $ do
    it "should have a working dot parser" $ do
        Visual.parse "a" `shouldBe` Right Dot
    -- it "should have a working connect parser" $ do
    --    Visual.parse "a -> b" `shouldBe` Right (Connect Dot Dot)
    -- it "should parse fix (a -> a) -> a" $ do
    --    Visual.parse "(a -> a) -> a" `shouldBe` Right (Connect (Group (Fix Dot)) Dot)