{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Buffer (Buffer)
import qualified Buffer
import Control.Monad (join)
import Data.Char (isNumber)
import Data.Foldable (fold, traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

data Command = Search Text | Persist Text | DumpBuffer | Help | Todo deriving stock (Eq, Show)

snippets :: FilePath
snippets = "/Users/sjurmillidahl/.hell/snippets"

todoF :: FilePath
todoF = "/Users/sjurmillidahl/.hell/todo"

main :: IO ()
main = do
    _print "h(askell) (sh)ell"
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
            _print $ "stored [" <> t <> "]"
        Help -> do
            _print "usage:"
            _print "persist: `hell p some kind of snippet`"
            _print "search: `hell s searchword"
            _print "dump: `hell d`"
        Todo -> todo

todo :: IO ()
todo = do
    oldlines <- readFileAsLines todoF
    let presentedLines = presentLines oldlines
    traverse_ print presentedLines
    _print "todo (q to exit)"
    input <- TIO.getLine
    if input == "q"
        then pure ()
        else do
            let newlines =
                    if all isNumber (T.unpack input)
                        then
                            let i = read @Int (T.unpack input)
                             in unpresentLines $ filter (\(j, _) -> j /= i) presentedLines
                        else oldlines <> [input]
            TIO.writeFile todoF (T.unlines newlines)
            todo

_print :: Text -> IO ()
_print = putStrLn . T.unpack

presentLines :: [a] -> [(Int, a)]
presentLines = zip [0 :: Int ..]

unpresentLines :: [(Int, a)] -> [a]
unpresentLines = fmap snd

parseCmd :: [String] -> Command
parseCmd [] = Help
parseCmd (c : l) = case c of
    "p" -> Persist (mkLine l)
    "s" -> Search (mkLine l)
    "d" -> DumpBuffer
    "todo" -> Todo
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