{-# LANGUAGE OverloadedStrings #-}

module VisualSpec where

import Visual

import Test.Hspec

spec :: Spec
spec = describe "Parser" $ do
    it "should have a working dot parser" $ do
        Visual.parse "a" `shouldBe` Right (Dot "a")
    it "should visualize f a as (.)" $ do
        Visual.parse "f a" `shouldBe` Right (Embellish (Dot "a"))
    it "should visualize >>=" $ do
        fmap Visual.render (Visual.parse "m a -> (a -> m b) -> m b") `shouldBe` Right "(.)--{.--(.)}--(.)"
    it "should visualize fix" $ do
        Visual.parse "(a -> a) -> a" `shouldBe` Right (Fix (Dot "a"))
    it "should visualize bitraverse" $ do
        fmap Visual.render (Visual.parse "(a -> f c) -> (b -> f d) -> t a b -> f (t c d)") `shouldBe` Right "{.--(.)}--{.--(.)}--(.)--({(.)})"
    it "should allow explicit existential quantification with forall keyword" $ do
        Visual.parse "forall a b. a -> b" `shouldBe` Right (Connect (Dot "a") (Dot "b"))
    it "should tolerate forall and contraints in forall a b. Functor f => f a" $ do
        Visual.parse "forall a b. Functor f => f a" `shouldBe` Right (Embellish (Dot "a"))