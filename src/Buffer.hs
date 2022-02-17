module Buffer (Buffer (..), empty, fromList, get, dump) where

import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MM
import Data.Text (Text)
import qualified Data.Text as T

newtype Buffer = Buffer {unBuffer :: MultiMap Text Text}

empty :: Buffer
empty = Buffer MM.empty

fromList :: [(Text, Text)] -> Buffer
fromList keyvals = Buffer (MM.fromList (filter nonempty keyvals))
    where nonempty (k,v) = not $ T.null k || T.null v

get :: Text -> Buffer -> [Text]
get k (Buffer mm) = MM.lookup k mm

dump :: Buffer -> [(Text, Text)]
dump (Buffer mm) = MM.toList mm