module Main where

import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.State.Lazy as State
import Data.Binary.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (fold)
import Data.String (IsString)
import qualified Data.Text as StrictText
import qualified Data.Text.Encoding as TE
import Data.Text.Lazy (Text, fromStrict, toStrict)
import qualified Data.Text.Lazy.Encoding as LTE
import Debug.Trace (traceShowId)
import Graphics.Svg
import qualified Hoogle
import qualified NeatInterpolation as NI
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.URI as Uri
import qualified Parser
import System.Environment (lookupEnv)
import System.IO (
    BufferMode (LineBuffering),
    hSetBuffering,
    stdout,
 )
import qualified Visual
import Web.Scotty

-- heroku provides PORT
readPort :: IO Int
readPort = do
    maybe 3000 (read @Int) <$> lookupEnv "PORT"

container :: [Attribute]
container = [Version_ <<- "1.1", Width_ <<- "1200", Height_ <<- "140"]

blobble :: Visual.Blobble
blobble = Visual.Blobble{x = 3, y = 3, w = 800, r = 50}

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    manager <- newManager tlsManagerSettings{managerModifyRequest = \r -> pure $ r{requestHeaders = [("User-Agent", "type-depict.io/0.0.1")]}}
    putStrLn "Hello world, lets see what port"
    port <- readPort
    print port
    putStrLn "scotty webserver going up"
    scotty port $ do
        get "/" $ do
            let cnt = doctype <> with (svg11_ mempty) container
            html (mainHtml "(a -> m b) -> m a -> m b" (Content $ prettyText cnt))
        post "/" $ do
            let cnt = doctype <> with (svg11_ mempty) container
            html (mainHtml "(a -> m b) -> m a -> m b" (Content $ prettyText cnt))
        get "/style.css" $ do
            setHeader "Content-Type" "text/css; charset=utf-8"
            file "assets/style.css"
        get "/favicon.ico" $ do
            setHeader "Content-Type" "image/vnd.microsoft.icon"
            file "assets/favicon.ico"
        post "/submit" $ do
            liftIO $ putStrLn "submit"
            txt <- param "signature"
            let lazyBSEnc = toLazyByteString $ Uri.encodePathSegments [txt]
            redirect (traceShowId $ LTE.decodeUtf8 lazyBSEnc)
        post "/hoogle" $ do
            liftIO $ putStrLn "hoogle"
            needleP <- param "signature"
            let needle = toStrict $ LTE.decodeUtf8 needleP
            hoogleRes <- liftIO $ Hoogle.search manager needle
            either
                (\s -> html (mainHtml "a -> b" "<p class=\"red\">Sorry, hoogle did not respond ok</p>"))
                (redirect . fromStrict)
                hoogleRes
        get "/:xpr" $ do
            p <- param "xpr"
            case Uri.decodePathSegments p of
                [] -> html (mainHtml "a -> b" "<p class=\"red\">Sorry, expression query param did not decode</p>")
                (x : _) -> draw x

draw :: StrictText.Text -> ActionM ()
draw txt =
    case Parser.parse (traceShowId txt) of
        Left _ -> html (mainHtml "a -> b" "<p class=\"red\">Sorry, expression did not parse</p>")
        Right vis -> do
            let s = Visual.renderSvg blobble vis
                svg = State.evalState s Visual.initEnv
                res = doctype <> with (svg11_ svg) container
            html (mainHtml (Expr . fromStrict $ txt) (Content $ prettyText res))

newtype Expr = Expr Text
    deriving stock (Eq, Show)
    deriving newtype (IsString)

newtype Content = Content Text
    deriving stock (Eq, Show)
    deriving newtype (IsString)

type Html = Text

mainHtml :: Expr -> Content -> Html
mainHtml expr content = fold ["<!DOCTYPE html>", "<html lang=\"en\">", htmlHead, htmlBody expr content, "</html>"]

htmlHead :: Html
htmlHead =
    fold
        [ "<head>"
        , "<link rel=\"stylesheet\" type=\"text/css\" href=\"/style.css\" />"
        , "<title>Haskell Signature Visualizer</title>"
        , "<meta charset=\"utf-8\" />"
        , "</head>"
        ]

htmlBody :: Expr -> Content -> Html
htmlBody expr (Content content) = fold ["<body>", "<h1>", "Haskell Type Visualizer", "</h1>", htmlForm expr, "<div class=\"content air\">", content, "</div>", shareLink, credits, "</body>"]

htmlForm :: Expr -> Html
htmlForm (Expr expr) =
    let strictT = toStrict expr
     in fromStrict
            [NI.text|
         <form action="/submit" method="post">
            <label class="inputlabel" for="signature">Haskell Type Signature</label><br>
            <input type="text" id="signature" name="signature" class="air azure" size="90" autocomplete="off" value="$strictT"><br>
            <button type="submit" class="bluebg" title="Render the visualization in the input field above">Visualize</button>
            <button type="submit" class="greenbg" formaction="/hoogle" title="Hoogle a function name in the input field above">Hoogle</button>
            <button type="submit" class="snowbg" formaction="/" title="Clear visualization and reset page">Clear</button>
        </form>
    |]

credits :: Html
credits =
    fromStrict
        [NI.text|
        <p class="credits">Created by <a href="https://twitter.com/SjurMillidahl">Sjur Millidahl</a>, published at <a href="https://github.com/mariatsji/type-depict">GitHub</a></p>
    |]

shareLinkJs :: Html
shareLinkJs =
    fromStrict
        [NI.text|
    <script>
        function copyToClipboard(toCopy) {
            console.log('invoked')
            var el = document.createElement('textarea')
            el.value = toCopy
            el.setAttribute('readonly', '')
            el.style.position = 'absolute'
            el.style.left = '-9999px'
            document.body.appendChild(el)
            el.select()
            document.execCommand('copy')
            document.body.removeChild(el)
        }
    </script>
   |]

shareLink :: Html
shareLink =
    shareLinkJs
        <> fromStrict
            [NI.text|
        <Button
            variant="contained"
            size="large"
            onClick="copyToClipboard(window.location.href)"
            title="Copy this visualization url to the clipboard">
            Copy to Clipboard
        </Button>
   |]