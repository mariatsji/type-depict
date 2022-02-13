{-# LANGUAGE DerivingStrategies #-}

module Buffer (Buffer (..), empty, insert, get) where

import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MM
import Data.Text (Text)

newtype Buffer = Buffer {unBuffer :: MultiMap Text Text}

empty :: Buffer
empty = Buffer MM.empty

insert :: (Text, [Text]) -> Buffer -> Buffer
insert (k, ks) (Buffer mm) = Buffer (foldr (MM.insert k) MM.empty ks)

get :: Text -> Buffer -> [Text]
get k (Buffer mm) = MM.lookup k mm