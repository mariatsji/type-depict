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
import Data.Text.Lazy.Encoding (encodeUtf8)

-- heroku provides PORT
readPort :: IO Int
readPort = do
    maybe 3000 (read @Int) <$> lookupEnv "PORT"

container :: [Attribute]
container = [Version_ <<- "1.1", Width_ <<- "1500", Height_ <<- "140"]

blobble :: Float -> Visual.Blobble
blobble width = Visual.Blobble{x = 3, y = 3, w = width, r = 50}

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
            html $ mainHtml Nothing (Expr "") (Content "")
        post "/" $ do
            redirect "/"
        get "/style.css" $ do
            setHeader "Content-Type" "text/css; charset=utf-8"
            file "assets/style.css"
        get "/favicon.ico" $ do
            setHeader "Content-Type" "image/vnd.microsoft.icon"
            file "assets/favicon.ico"
        get "/robots.txt" $ do
            setHeader "Content-Type" "text/plain"
            file "assets/robots.txt"
        post "/submit" $ do
            liftIO $ putStrLn "submit"
            txt <- param "signature"
            let lazyBSEnc = toLazyByteString $ Uri.encodePathSegments [txt]
            redirect (traceShowId $ LTE.decodeUtf8 lazyBSEnc)
        post "/hoogle" $ do
            needleP <- param "hoogle"
            liftIO $ print "hooglin for"
            let needle = toStrict $ LTE.decodeUtf8 (traceShowId needleP)
            hoogleRes <- liftIO $ Hoogle.search manager needle
            liftIO $ print "hoogleRes:"
            liftIO $ print hoogleRes
            either
                (\s -> html (mainHtml (Just (Hoogle (fromStrict needle))) "a -> b" "<p class=\"red\">Sorry, hoogle did not respond ok</p>"))
                (redirect . fromStrict)
                hoogleRes
        get "/api/:xpr" $ do
            p <- param "xpr"
            case Uri.decodePathSegments p of
                [] -> mempty
                (x : _) -> do
                    setHeader "Content-Type" "image/svg+xml"
                    drawSvgOnly x
        get "/:xpr" $ do
            p <- param "xpr"
            case Uri.decodePathSegments p of
                [] -> html (mainHtml Nothing "" "<p class=\"red\">Sorry, expression query param did not decode</p>")
                (x : _) -> draw x

drawSvgOnly :: StrictText.Text -> ActionM ()
drawSvgOnly txt =
    case Parser.parse txt of
        Left _ -> html "<p class=\"red\">Sorry, expression did not parse</p>"
        Right vis -> do
            let initWidth = Visual.estimateWidth vis
                s = Visual.renderSvg (blobble initWidth) vis
                svg = State.evalState s Visual.initEnv
                res = doctype <> with (svg11_ svg) container
            raw . encodeUtf8 $ prettyText res

draw :: StrictText.Text -> ActionM ()
draw txt =
    case Parser.parse txt of
        Left _ -> html (mainHtml Nothing "" "<p class=\"red\">Sorry, expression did not parse</p>")
        Right vis -> do
            let initWidth = Visual.estimateWidth vis
                s = Visual.renderSvg (blobble initWidth) vis
                svg = State.evalState s Visual.initEnv
                res = doctype <> with (svg11_ svg) container
            html (mainHtml Nothing (Expr . fromStrict $ txt) (Content $ prettyText res))

newtype Expr = Expr Text
    deriving stock (Eq, Show)
    deriving newtype (IsString)

newtype Hoogle = Hoogle Text
    deriving stock (Eq, Show)
    deriving newtype (IsString)

newtype Content = Content Text
    deriving stock (Eq, Show)
    deriving newtype (IsString)

type Html = Text

mainHtml :: Maybe Hoogle -> Expr -> Content -> Html
mainHtml hoogleM expr content = fold ["<!DOCTYPE html>", "<html lang=\"en\">", htmlHead, htmlBody hoogleM expr content, "</html>"]

htmlHead :: Html
htmlHead =
    fold
        [ "<head>"
        , "<link rel=\"stylesheet\" type=\"text/css\" href=\"/style.css\" />"
        , "<title>type-depict.io</title>"
        , "<meta charset=\"utf-8\" />"
        , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
        , "</head>"
        ]

htmlBody :: Maybe Hoogle -> Expr -> Content -> Html
htmlBody hoogleM expr (Content content) = fold ["<body>", "<h1>", "Haskell Type Visualizer", "</h1>", htmlForm hoogleM expr, "<div class=\"content\">", content, "</div>", shareLink, credits, "</body>"]

htmlForm :: Maybe Hoogle -> Expr -> Html
htmlForm mh (Expr expr) =
    let strictT = toStrict expr
        strictH = maybe "" (\(Hoogle hoogle) -> toStrict hoogle) mh
        disableS = if StrictText.null strictT then "disabled" else ""
     in fromStrict
            [NI.text|
         <form action="/submit" method="post">
            <label class="inputlabel" for="hoogle">Search for function name</label><br />
            <input type="text"
                id="hoogle" name="hoogle"
                class="azure"
                size="30"
                autocomplete="off"
                placeholder="traverse"
                value="$strictH"
                onkeyup="if(this.value.length > 0) document.getElementById('hoogleBtn').disabled = false; else document.getElementById('hoogleBtn').disabled = true;"/><br />
            <button
                id="hoogleBtn" type="submit"
                class="greenbg"
                formaction="/hoogle"
                title="Hoogle a function name in the input field above"
                disabled>Hoogle</button><br />
            <label class="inputlabel" for="signature">.. or provide a custom type signature</label><br />
            <input type="text"
                id="signature" name="signature"
                class="azure"
                size="90"
                autocomplete="off"
                placeholder="f (a -> b) -> f a -> f b"
                value="$strictT"
                onkeyup="if(this.value.length > 0) document.getElementById('submitBtn').disabled = false; else document.getElementById('submitBtn').disabled = true;" /><br />
            <button type="submit" id="submitBtn" class="bluebg" title="Render the visualization in the input field above" $disableS>Visualize</button>
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
            console.log('invoked');
            var el = document.createElement('textarea');
            el.value = toCopy;
            el.setAttribute('readonly', '');
            el.style.position = 'absolute';
            el.style.left = '-9999px';
            document.body.appendChild(el);
            el.select();
            document.execCommand('copy');
            document.body.removeChild(el);
            window.alert("URL copied to clipboard");
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
            class="sharelink"
            size="large"
            onClick="copyToClipboard(window.location.href)"
            title="Copy this visualization url to the clipboard">
            Sharelink
        </Button>
   |]