module Hoogle (search) where

import Data.Aeson (FromJSON, eitherDecode)

import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import qualified NeatInterpolation as NI
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

newtype HoogleRes = HoogleRes
    { item :: Text
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON HoogleRes

search :: Manager -> Text -> IO (Either String Text)
search manager needle = do
    let needle' = case T.split (== ' ') needle of
            x : _ -> x
            _ -> "id"
        url = searchUrl needle'
    request <- parseRequest url
    withResponse request manager $ do
        \responseBodyR -> do
            bs <- responseBody responseBodyR
            case eitherDecode @[HoogleRes] (fromStrict bs) of
                Right (HoogleRes{..} : _) -> pure $ Right item
                Right [] -> pure $ Left "no results"
                Left s -> pure $ Left s

searchUrl :: Text -> String
searchUrl t =
    T.unpack [NI.text|https://hoogle.haskell.org?mode=json&format=text&hoogle=$t&start=1&count=1|]

testIt :: IO ()
testIt = do
    manager <- newManager tlsManagerSettings{managerModifyRequest = \r -> pure $ r{requestHeaders = [("User-Agent", "type-depict.io/0.0.1")]}}
    res <- search manager "traverse"
    print res