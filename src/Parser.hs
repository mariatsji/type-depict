{-#language OverloadedLists #-}
module Parser where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text (Text)
import qualified Data.Text as T
import Visual
import Data.Text.Encoding (decodeUtf32BE)

parse :: Text -> Either String Visual
parse = A.parseOnly visParser . strip

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
    dropForall t' = maybeOnlyAfter t' " =>"

maybeOnlyAfter :: Text -> Text -> Text
maybeOnlyAfter t needle = case T.splitOn needle t of
    [x] -> x
    (_ : ts) -> T.intercalate "" ts
    [] -> t

fixParser :: Parser Visual
fixParser = do
    c <- A.try connectParser
    case c of
        (Connect [Connect [a, b], c]) -> if b == c then pure (Fix a) else fail ""
        (Connect [Group (Connect [a, b]), c]) -> if b == c then pure (Fix a) else fail ""
        _ -> fail ""

connectParser :: Parser Visual
connectParser = do
    a <- A.skipSpace *> A.try connectable
    bs <- A.many1 $ do
        _ <- A.skipSpace >> A.string "->" >> A.skipSpace
        connectable
    pure $ Connect (a :| bs)
  where
    connectable = tupleParser <|> groupParser <|> embellishParser <|> dotParser <|> listParser

embellishParser :: Parser Visual
embellishParser = do
    _ <- A.skipSpace
    w <- wordspace
    e <- A.many1 $ do
        _ <- A.skipSpace
        embellishable
    pure . Embellish (Just w) . NE.fromList $ e
  where
    embellish1 = wordspace >> embellishable
    embellishable = groupParser <|> dotParser <|> listParser <|> tupleParser
    wordspace = word <* A.space

listParser :: Parser Visual
listParser =
    Embellish Nothing <$> do
        _ <- A.skipSpace
        _ <- A.char '[' *> A.many' A.space
        x <- listable
        _ <- A.many' A.space <* A.char ']'
        pure (NE.fromList [x])
  where
    listable = connectParser <|> embellishParser <|> dotParser <|> groupParser <|> tupleParser

word :: Parser String
word = A.many1 A.letter

tupleParser :: Parser Visual
tupleParser = do
    _ <- A.skipSpace
    _ <- A.char '('
    _ <- A.skipSpace
    ts <- A.many1 $ tupable <* A.skipSpace <* A.char ',' <* A.skipSpace
    t <- tupable
    _ <- A.skipSpace
    _ <- A.char ')'
    pure $ Embellish Nothing (NE.fromList (ts <> [t]))
  where
    tupable = connectParser <|> embellishParser <|> tupleParser <|> listParser <|> dotParser

groupParser :: Parser Visual
groupParser =
    Group <$> do
        _ <- A.skipSpace
        _ <- A.char '(' *> A.many' A.space
        x <- groupable
        _ <- A.many' A.space <* A.char ')'
        pure x
  where
    groupable = fixParser <|> connectParser <|> embellishParser <|> dotParser <|> listParser <|> tupleParser

dotParser :: Parser Visual
dotParser = do
    _ <- A.skipSpace
    ws <- word <|> T.unpack <$> A.string "()"
    pure $ Dot ws

