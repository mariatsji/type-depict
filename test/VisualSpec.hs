{-# language OverloadedStrings #-}

module VisualSpec where

import Visual

import Test.Hspec

spec :: Spec
spec = describe "Parser" $ do
    it "should have a working dot parser" $ do
        Visual.parse "a" `shouldBe` Right ( Dot "a" )
    it "should visualize >>=" $ do
        fmap Visual.render (Visual.parse "m a -> (a -> m b) -> m b") `shouldBe` Right "(.)--{.--(.)}--(.)"
    it "should visualize fix" $ do
        Visual.parse "(a -> a) -> a" `shouldBe` Right ( Fix ( Dot "a" ) )