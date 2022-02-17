{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Buffer (Buffer)
import qualified Buffer
import Data.Foldable (fold, traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Control.Monad (join)

data Command = Search Text | Persist Text | DumpBuffer | Help deriving stock (Eq, Show)

snippets :: FilePath
snippets = "/Users/sjurmillidahl/.hell/snippets"

main :: IO ()
main = do
    print "h(askell) (sh)ell"
    args <- getArgs
    snippetlines <- readFileAsLines snippets
    shelllines <- readFilesAsLines ["/Users/sjurmillidahl/.zsh_history", "/Users/sjurmillidahl/.bash_history"]
    let buffer = Buffer.fromList (keyalize <$> shelllines <> snippetlines)
    case parseCmd args of
        DumpBuffer ->
            traverse_ print (Buffer.dump buffer)
        Search t ->
            traverse_ (\v -> print (t <> " " <> v)) (Buffer.get t buffer)
        Persist t -> do
            TIO.writeFile snippets (T.unlines (t : snippetlines))
            print $ "stored [" <> t <> "]"
        Help -> do
            print "usage:"
            print "persist: `hell p some kind of snippet`"
            print "search: `hell s searchword"
            print "dump: `hell d`"

parseCmd :: [String] -> Command
parseCmd [] = Help
parseCmd (c:l) = case c of
    "p" -> Persist (mkLine l)
    "s" -> Search (mkLine l)
    "d" -> DumpBuffer
    _ -> Help

mkLine :: [String] -> Text
mkLine s = T.strip . T.intercalate " " $ T.pack <$> s 

readFilesAsLines :: [FilePath] -> IO [Text]
readFilesAsLines l = join <$> traverse readFileAsLines l

readFileAsLines :: FilePath -> IO [Text]
readFileAsLines = fmap T.lines . TIO.readFile

keyalize :: Text -> (Text, Text)
keyalize t = case T.words t of
    [] -> ("", "")
    w : ws -> (w, T.intercalate " " ws)