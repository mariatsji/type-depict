{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Visual where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, (<?>))
import qualified Data.Attoparsec.Text as A
import Data.Text (Text)
import qualified Data.Text as T

data Visual
    = Fix Visual
    | Connect Visual Visual
    | Embellish Visual
    | Group Visual
    | Dot
    deriving stock (Eq)

instance Show Visual where
    show = T.unpack . render

parse :: Text -> Either String Visual
parse = A.parseOnly visParser

visParser :: Parser Visual
visParser = fixParser <|> embellishParser <|> groupParser <|> dotParser <|> connectParser <?> "no Visualization"

fixParser :: Parser Visual
fixParser = do
    c <- A.letter
    _ <- A.space >> A.string "->" >> A.space >> A.char c
    pure (Fix Dot) <?> "no fix"

connectParser :: Parser Visual
connectParser = do
    a <- A.try connectableA
    _ <- A.space >> A.string "->" >> A.space
    b <- connectableB
    pure $ Connect a b
  where
    connectableA = groupParser <|> embellishParser <|> dotParser
    connectableB = connectableA <|> connectParser

embellishParser :: Parser Visual
embellishParser = Embellish <$> (embellish4 <|> embellish3 <|> embellish2 <|> embellish1) <?> "no embellish"
  where
    embellish1 = A.letter >> A.space >> embellishable
    embellish2 = A.letter >> A.space >> A.letter >> A.space >> embellishable
    embellish3 = A.letter >> A.space >> A.letter >> A.space >> A.letter >> A.space >> embellishable
    embellish4 = A.letter >> A.space >> A.letter >> A.space >> A.letter >> A.space >> A.letter >> A.space >> embellishable
    embellishable = groupParser <|> dotParser <|> connectParser

groupParser :: Parser Visual
groupParser =
    Group <$> do
        _ <- A.char '(' *> A.space
        x <- groupable
        _ <- A.space <* A.char ')'
        pure x
  where
    groupable = connectParser <|> embellishParser <|> dotParser

dotParser :: Parser Visual
dotParser = Dot <$ A.letter <?> "no dot"

render :: Visual -> Text
render = \case
    Dot -> "."
    Connect a b -> render a <> "--" <> render b
    Embellish a -> "(" <> render a <> ")"
    Fix a -> "@" <> render a
    Group a -> "{" <> render a <> "}"

-- (a -> a) -> a
fixV :: Visual
fixV = Connect (Group (Fix Dot)) Dot

-- m a -> (a -> m b) -> m b
bindV :: Visual
bindV = Connect (Connect (Embellish Dot) (Group (Connect Dot (Embellish Dot)))) (Embellish Dot)

-- f a -> f a
foreverV = Fix . Embellish