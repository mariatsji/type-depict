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
    | Dot String
    deriving stock (Eq)

instance Show Visual where
    show = T.unpack . render

parse :: Text -> Either String Visual
parse = A.parseOnly visParser

-- todo constraints
visParser :: Parser Visual
visParser = forallParser *> (fixParser <|> connectParser <|> embellishParser <|> groupParser <|> dotParser)
  where
    forallParser = A.many' $ do
        _ <- A.string "forall" *> A.space
        _ <- A.takeWhile (/= '.')
        _ <- A.char '.' *> A.many' A.space
        pure ()

fixParser :: Parser Visual
fixParser = do
    c <- A.try connectParser
    case c of
        (Connect (Group (Connect a b)) c) -> if b == c then pure (Fix a) else fail ""
        _ -> fail ""

connectParser :: Parser Visual
connectParser = do
    a <- A.try connectableA
    _ <- A.space >> A.string "->" >> A.space
    b <- connectableB
    pure $ Connect a b
  where
    connectableA = groupParser <|> embellishParser <|> dotParser
    connectableB = connectParser <|> connectableA

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
        _ <- A.char '(' *> A.many' A.space
        x <- groupable
        _ <- A.many' A.space <* A.char ')'
        pure x
  where
    groupable = connectParser <|> embellishParser <|> dotParser

dotParser :: Parser Visual
dotParser = do
    w <- A.many1 A.letter
    pure $ Dot w

render :: Visual -> Text
render = \case
    Dot _ -> "."
    Connect a b -> render a <> "--" <> render b
    Embellish a -> "(" <> render a <> ")"
    Fix a -> "@" <> render a
    Group a -> "{" <> render a <> "}"
