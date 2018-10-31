#!/bin/env stack
{-
  stack script --resolver=lts-11.13
    --package split
    --package containers
-}
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Traversable (for)
import System.Environment (getArgs)
import System.IO (BufferMode(..), hSetBuffering, stdout)

data Entry =
  Entry { category :: String, price :: Integer } deriving (Eq, Show)

main :: IO ()
main = do
  files <- getArgs
  entries <-
    for files (\path -> do
      contents <- readFile path
      return (map toTuple (parseEntries contents))
    )
  let summary = Map.fromListWith (\x y -> x + y) (concat entries)
  putStr (formatSummry summary)

toTuple :: Entry -> (String, Integer)
toTuple entry = (category entry, price entry)

parseEntries :: String -> [Entry]
parseEntries s = map parseEntry (lines s)

parseEntry :: String -> Entry
parseEntry s =
  case splitOn "\t" s of
    [c, sv] -> Entry c (read sv)
    _ -> error ("Invalid entry: " ++ show s)

formatSummry :: Map String Integer -> String
formatSummry summary = unlines (map formatSummaryItem (Map.toList summary))

formatSummaryItem :: (String, Integer) -> String
formatSummaryItem (cat, total) = cat ++ "\t" ++ show total
