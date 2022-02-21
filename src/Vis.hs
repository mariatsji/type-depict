{-# language DerivingStrategies, LambdaCase, OverloadedStrings #-}

module Vis where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text as A
import Data.Attoparsec.Text (Parser)
import Data.Text (Text)

data Vis = Fix Vis
         | Connect Vis Vis
         | Embellish Vis
         | Group Vis
         | Dot
    deriving stock Eq

parse :: Text -> Either String Vis
parse = A.parseOnly visParser

visParser = fixParser <|> connectParser <|> embellishParser <|> groupParser <|> dotParser

fixParser :: Parser Vis
fixParser = dotToDot -- <|> embellishToEmbellish
    where dotToDot = do
                    c <- A.letter
                    _ <- A.space >> A.string "->" >> A.space >> A.char c
                    pure Dot

connectParser :: Parser Vis
connectParser = do
    a <- visParser
    _ <- A.space >> A.string "->" >> A.space
    Connect a <$> visParser

embellishParser :: Parser Vis
embellishParser = Embellish <$> (A.letter >> A.space >> visParser)

groupParser :: Parser Vis
groupParser = Group <$> (A.char '(' *> visParser <* A.char ')')

dotParser :: Parser Vis
dotParser = Dot <$ A.letter

render :: Vis -> Text
render = \case
    Dot -> "."
    Connect a b -> render a <> "--" <> render b
    Embellish a -> "(" <> render a <> ")"
    Fix a -> "@" <> render a
    Group a -> "{" <> render a <> "}"

-- (a -> a) -> a
fixV :: Vis
fixV = Connect (Group (Fix Dot)) Dot

-- m a -> (a -> m b) -> m b
bindV :: Vis
bindV = Connect (Connect (Embellish Dot) (Group (Connect Dot (Embellish Dot)))) (Embellish Dot)

-- f a -> f a
foreverV = Fix . Embellish