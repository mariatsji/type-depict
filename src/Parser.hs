module Parser where

import Buffer (Buffer)
import qualified Buffer
import Control.Applicative (many, (<|>))
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.Char (isSpace)
import Data.Text (Text)

parse :: Text -> Either String Buffer
parse t = Buffer.fromList <$> A.parseOnly (many entryParser) t

entryParser :: Parser (Text, Text)
entryParser = do
    k <- A.takeWhile (not . isSpace)
    _ <- A.skipSpace
    v <- A.takeTill A.isEndOfLine <* (A.endOfLine <|> A.endOfInput)
    pure (k, v)
