module Parser where

import Buffer (Buffer)
import qualified Buffer
import Control.Applicative (many)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.Char (isSpace)
import Data.Text (Text)

parse :: Text -> Either String Buffer
parse t = A.parseOnly parser t

wordParser :: Parser Text
wordParser = A.takeWhile (not . isSpace)

lineParser :: Parser (Text, [Text])
lineParser = do
    k <- wordParser
    ks <- many wordParser
    _ <- A.endOfLine
    pure (k, ks)

parser :: Parser Buffer
parser = foo <$> many lineParser
  where
    foo lines = foldr Buffer.insert Buffer.empty lines
