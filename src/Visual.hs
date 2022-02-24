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

render :: Visual -> Text
render = \case
    Dot _ -> "."
    Connect a b -> render a <> "--" <> render b
    Embellish a -> "(" <> render a <> ")"
    Fix a -> "@" <> render a
    Group a -> "{" <> render a <> "}"

instance Show Visual where
    show = T.unpack . render

parse :: Text -> Either String Visual
parse = A.parseOnly visParser . strip

-- todo constraints
visParser :: Parser Visual
visParser = fixParser <|> connectParser <|> embellishParser <|> groupParser <|> dotParser <|> listParser <|> tupleParser

strip :: Text -> Text
strip t =
    if T.null t
        then t
        else T.strip $ dropSignature $ dropForall $ dropConstraints t
  where
    dropSignature t' = maybeOnlyAfter t' "::"
    dropConstraints t' = maybeOnlyAfter t' "."
    dropForall t' = maybeOnlyAfter t' "=>"

maybeOnlyAfter :: Text -> Text -> Text
maybeOnlyAfter t needle = case T.splitOn needle t of
    [x] -> x
    (_ : ts) -> T.intercalate "" ts
    [] -> t

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
    connectableA = groupParser <|> embellishParser <|> dotParser <|> listParser  <|> tupleParser
    connectableB = connectParser <|> connectableA

embellishParser :: Parser Visual
embellishParser = Embellish <$> (embellish4 <|> embellish3 <|> embellish2 <|> embellish1) <?> "no embellish"
  where
    embellish1 = word >> A.space >> embellishable
    embellish2 = word >> A.space >> word >> A.space >> embellishable
    embellish3 = word >> A.space >> word >> A.space >> word >> A.space >> embellishable
    embellish4 = word >> A.space >> word >> A.space >> word >> A.space >> word >> A.space >> embellishable
    embellishable = groupParser <|> dotParser <|> connectParser <|> listParser <|> tupleParser

listParser :: Parser Visual
listParser =
    Embellish <$> do
        _ <- A.char '[' *> A.many' A.space
        x <- listable
        _ <- A.many' A.space <* A.char ']'
        pure x
  where
    listable = connectParser <|> embellishParser <|> dotParser <|> groupParser <|> tupleParser

word :: Parser String
word = A.many1 A.letter

tupleParser :: Parser Visual
tupleParser =
    Embellish
        <$> ( A.skipSpace
                *> A.char '('
                *> A.many1 (A.letter <|> A.space)
                *> A.char ','
                *> tupable <* A.char ')'
            )
  where
    tupable = connectParser <|> embellishParser <|> tupleParser <|> listParser <|> dotParser

groupParser :: Parser Visual
groupParser =
    Group <$> do
        _ <- A.char '(' *> A.many' A.space
        x <- groupable
        _ <- A.many' A.space <* A.char ')'
        pure x
  where
    groupable = connectParser <|> embellishParser <|> dotParser <|> listParser

dotParser :: Parser Visual
dotParser = Dot <$> word
