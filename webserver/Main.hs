module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (fold)
import qualified Data.Text as StrictText
import Data.Text.Lazy (Text, fromStrict, toStrict)
import qualified Data.Text.Lazy.Encoding as TE
import Debug.Trace (traceShowId)
import Graphics.Svg
import qualified Hoogle
import qualified NeatInterpolation as NI
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Parser
import qualified Visual
import Web.Scotty

import System.Environment (lookupEnv)
import System.IO (
    BufferMode (LineBuffering),
    hSetBuffering,
    stdout,
 )

-- heroku provides PORT
readPort :: IO Int
readPort = do
    maybe 3000 (read @Int) <$> lookupEnv "PORT"

container :: [Attribute]
container = [Version_ <<- "1.1", Width_ <<- "2000", Height_ <<- "300"]

blobble :: Visual.Blobble
blobble = Visual.Blobble{x = 5, y = 5, w = 1000, r = 40}

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    manager <- newManager tlsManagerSettings
    putStrLn "Hello world, lets see what port"
    port <- readPort
    print port
    putStrLn "scotty webserver going up"
    scotty port $ do
        get "/" $ do
            html $ mainHtml "(a -> m b) -> m a -> m b" ""
        get "/style.css" $ do
            setHeader "Content-Type" "text/css; charset=utf-8"
            file "assets/style.css"
        post "/submit" $ do
            liftIO $ putStrLn "submit"
            expression <- param "signature"
            let txt = toStrict $ TE.decodeUtf8 expression
            case Parser.parse (traceShowId txt) of
                Left _ -> html (mainHtml "a -> b" "<p class=\"red\">Sorry, expression did not parse</p>")
                Right vis -> do
                    let svg = doctype <> with (svg11_ (Visual.renderSvg blobble vis)) container
                    html (mainHtml (fromStrict txt) (prettyText svg))
        post "/hoogle" $ do
            liftIO $ putStrLn "hoogle"
            needleP <- param "signature"
            let needle = toStrict $ TE.decodeUtf8 needleP
            hoogleRes <- liftIO $ Hoogle.search manager needle
            either
                (\s -> html (mainHtml "a -> b" "<p class=\"red\">Sorry, hoogle did not respond ok</p>"))
                ( \txt ->
                    case Parser.parse (traceShowId txt) of
                        Left _ -> html (mainHtml "a -> b" "<p class=\"red\">Sorry, hoogle-result did not parse</p>")
                        Right vis -> do
                            let svg = doctype <> with (svg11_ (Visual.renderSvg blobble vis)) container
                            html (mainHtml (fromStrict txt) (prettyText svg))
                )
                hoogleRes

mainHtml :: Text -> Text -> Text
mainHtml expr content = fold ["<!DOCTYPE html>", "<html lang=\"en\">", htmlHead, htmlBody expr content, "</html>"]

htmlHead :: Text
htmlHead =
    fold
        [ "<head>"
        , "<link rel=\"stylesheet\" type=\"text/css\" href=\"/style.css\" />"
        , "<title>Haskell Signature Visualizer</title>"
        , "<meta charset=\"utf-8\" />"
        , "</head>"
        ]

htmlBody :: Text -> Text -> Text
htmlBody expr content = fold ["<body>", "<h1>", "Haskell Expression Visualizer", "</h1>", htmlForm expr, content, "</body>"]

htmlForm :: Text -> Text
htmlForm expr =
    let strictT = toStrict expr
     in fromStrict
            [NI.text|
         <form action="/submit" method="post">
            <label for="signature">Haskell Type Signature</label><br>
            <input type="text" id="signature" name="signature" size="90" value="$strictT"><br>
            <button type="submit">Visualize</button>
            <button type="submit" formaction="/hoogle">Hoogle</button>
        </form> 
    |]
