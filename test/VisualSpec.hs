{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module VisualSpec where

import Parser
import Visual

import Test.Hspec

spec :: Spec
spec = describe "Parser" $ do
    it "should have a working dot parser" $ do
        Parser.parse "a" `shouldBe` Right (Dot "a")
    it "should visualize f a as (.)" $ do
        Parser.parse "f a" `shouldBe` Right (Embellish (Just "f") [Dot "a"])
    it "should visualize >>=" $ do
        fmap Visual.render (Parser.parse "m a -> (a -> m b) -> m b") `shouldBe` Right "(.)--{.--(.)}--(.)"
    it "should visualize fix" $ do
        Parser.parse "(a -> a) -> a" `shouldBe` Right (Fix (Dot "a"))
    it "should visualize bitraverse" $ do
        fmap Visual.render (Parser.parse "(a -> f c) -> (b -> f d) -> t a b -> f (t c d)") `shouldBe` Right "{.--(.)}--{.--(.)}--(..)--({(..)})"
    it "should allow explicit existential quantification with forall keyword" $ do
        Parser.parse "forall a b. a -> b" `shouldBe` Right (Connect [Dot "a", Dot "b"])
    it "should tolerate forall and contraints in forall a b. Functor f => f a" $ do
        Parser.parse "forall a b. Functor f => f a" `shouldBe` Right (Embellish (Just "f") [Dot "a"])
    it "should tolerate a function name in pure :: forall a b. Functor f => f a" $ do
        Parser.parse "pure :: forall a b. Functor f => f a" `shouldBe` Right (Embellish (Just "f") [Dot "a"])
    it "should tolerate only function name in f :: a" $ do
        Parser.parse "f :: a" `shouldBe` Right (Dot "a")
    it "should accept non-polymorphic types  maybe :: Decoder a -> Decoder (Maybe a)" $ do
        fmap Visual.render (Parser.parse "maybe :: Decoder a -> Decoder (Maybe a)") `shouldBe` Right "(.)--({(.)})"
    it "should accept non-polymorphic types in e.g. String -> String" $ do
        Parser.parse "String -> String" `shouldBe` Right (Connect [Dot "String", Dot "String"])
    it "understand e.g. non-polymorphic Either functions either :: (String -> Text) -> (Int -> Float) -> Either String Int -> Text" $ do
        fmap Visual.render (Parser.parse "either :: (String -> Text) -> (Int -> Float) -> Either String Int -> Text") `shouldBe` Right "{.--.}--{.--.}--(..)--."
    it "understand lists as embellishments" $ do
        Parser.parse "[a]" `shouldBe` Right (Embellish Nothing [Dot "a"])
    it "understands applicative" $ do
        fmap Visual.render (Parser.parse "f ( a -> b ) -> f a -> f b") `shouldBe` Right "({.--.})--(.)--(.)"
    it "understand complicated list embellishments" $ do
        fmap Visual.render (Parser.parse "[(a -> b)] -> [a] -> [b]") `shouldBe` Right "({.--.})--(.)--(.)"
    it "understands a simple tuple (a,b)" $ do
        fmap Visual.render (Parser.parse "(a,b)") `shouldBe` Right "(..)"
    it "understands a complicated tuple (a,(a,(c,d)))" $ do
        fmap Visual.render (Parser.parse "(a,(a,(c,d)))") `shouldBe` Right "(.(.(..)))"
    it "understands a tuple with a connectable (a, f -> (m [a]))" $ do
        fmap Visual.render (Parser.parse "(a, f -> (m [a]))") `shouldBe` Right "(..--{((.))})"
    it "parses (a,b,c)" $ do
        Parser.parse "(a,b,c)" `shouldBe` Right (Embellish Nothing [Dot "a", Dot "b", Dot "c"])