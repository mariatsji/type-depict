module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (fold)
import qualified Data.Text as StrictText
import Data.Text.Lazy (Text, fromStrict, toStrict)
import qualified Data.Text.Lazy.Encoding as TE
import Debug.Trace (traceShowId)
import Graphics.Svg
import qualified NeatInterpolation as NI
import qualified Parser
import qualified Visual
import Web.Scotty

import System.Environment (lookupEnv)
import System.IO
    ( stdout, hSetBuffering, BufferMode(LineBuffering) )

-- heroku provides PORT
readPort :: IO Int
readPort = do
    maybe 3000 (read @Int) <$> lookupEnv "PORT"

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
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
                    let container = [Version_ <<- "1.1", Width_ <<- "2000", Height_ <<- "300"]
                        blobble = Visual.Blobble{x = 5, y = 5, w = 1000, r = 40}
                        svg = doctype <> with (svg11_ (Visual.renderSvg blobble vis)) container
                    html (mainHtml (fromStrict txt) (prettyText svg))

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
            <input type="text" id="signature" name="signature" size="70" value="$strictT"><br>
            <input type="submit" value="Visualize">
        </form> 
    |]
