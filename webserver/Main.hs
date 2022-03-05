module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (fold)
import Data.Text.Lazy (Text, fromStrict, toStrict)
import qualified Data.Text as StrictText
import qualified Data.Text.Lazy.Encoding as TE
import Debug.Trace (traceShowId)
import Graphics.Svg
import qualified NeatInterpolation as NI
import qualified Parser
import qualified Visual
import Web.Scotty

main :: IO ()
main = do
    putStrLn "scotty webserver :3000"
    scotty 3000 $ do
        get "/" $ do
            html $ mainHtml "(a -> m b) -> m a -> m b" ""
        get "/style.css" $ do
            setHeader "Content-Type" "text/css; charset=utf-8"
            file "webserver/assets/style.css"
        post "/submit" $ do
            liftIO $ putStrLn "submit"
            expression <- param "signature"
            let txt = toStrict $ TE.decodeUtf8 expression
            case Parser.parse (traceShowId txt) of
                Left _ -> html (mainHtml "a -> b" "<p class=\"error\">Sorry, expression did not parse</p>")
                Right vis -> do
                    let container = [Version_ <<- "1.1", Width_ <<- "1000", Height_ <<- "300"]
                        blobble = Visual.Blobble{x = 5, y = 5, w = 600, r = 30}
                        svg = doctype <> with (svg11_ (Visual.renderSvg blobble vis)) container
                    html (mainHtml (fromStrict txt) (prettyText svg))

mainHtml :: Text -> Text -> Text
mainHtml expr content = fold ["<!DOCTYPE html>", "<html lang=\"en\">", htmlHead, htmlBody expr content, "</html>"]

htmlHead :: Text
htmlHead = fold ["<head>", "<meta charset=\"utf-8\" />", "<link rel=\"stylesheet\" href=\"/style.css\">", "</head>"]

htmlBody :: Text -> Text -> Text
htmlBody expr content = fold ["<body>", "<h1>", "Haskell Expression Visualizer", "</h1>", htmlForm expr, content, "</body>"]

htmlForm :: Text -> Text
htmlForm expr =
    let strictT = toStrict expr
    in fromStrict
        [NI.text|
         <form action="/submit" method="post">
            <label for="signature">Haskell Type Signature</label><br>
            <input type="text" id="signature" name="signature" value="$strictT"><br>
            <input type="submit" value="Visualize">
        </form> 
    |]
