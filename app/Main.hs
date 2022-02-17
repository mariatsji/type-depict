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

data Command = Search Text | Persist Text | DumpBuffer | Help deriving stock (Eq, Show)

snippets :: FilePath
snippets = "/Users/sjurmillidahl/.hell/snippets"

main :: IO ()
main = do
    print "h(askell) (sh)ell"
    args <- getArgs
    lines <- readFilesAsLines [snippets, "/Users/sjurmillidahl/.zsh_history", "/Users/sjurmillidahl/.bash_history"]
    let buffer = Buffer.fromList (keyalize <$> lines)
    case parseCmd (fold args) of
        DumpBuffer ->
            traverse_ println (Buffer.dump buffer)
        Search t ->
            traverse_ println (Buffer.get t buffer)
        Persist t -> do
            TIO.writeFile snippets (T.intercalate "" (t : lines))
            print $ "stored [" <> t <> "]"
        Help -> do
            print "usage:"
            print "persist: `hell p some kind of snippet`"
            print "search: `hell s searchword"
            print "dump: `hell d`"

parseCmd :: String -> Command
parseCmd (c:l) = case c of
    'p' -> Persist (T.strip $ T.pack l)
    's' -> Search (T.strip $ T.pack l)
    'd' -> DumpBuffer
    _ -> Help
parseCmd _ = Help

readFilesAsLines :: [FilePath] -> IO [Text]
readFilesAsLines = traverse (fmap (T.intercalate " ") . readFileAsLines)

readFileAsLines :: FilePath -> IO [Text]
readFileAsLines = fmap T.lines . TIO.readFile

keyalize :: Text -> (Text, Text)
keyalize t = case T.words t of
    [] -> ("", "")
    w : ws -> (w, T.intercalate " " ws)

println :: Show a => a -> IO ()
println t = do
    print t
    print "\n"