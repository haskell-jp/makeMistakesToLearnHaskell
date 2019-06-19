#!/bin/env stack
{-
  stack script --resolver=lts-11.13
    --package split
    --package containers
-}
import Data.List.Split
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import Data.Traversable (for)
import System.Environment (getArgs)
import System.IO (BufferMode(..), hSetBuffering, stdout)
import Text.Read (readMaybe)

data Entry =
  Entry { category :: String, price :: Integer } deriving (Eq, Show)

main = do
  files <- getArgs
  entries <-
    for files $ \path ->
      map toTuple . parseEntries <$> readFile path
  let summary = Map.fromListWith (+) (concat entries)
  putStr $ formatSummry summary

toTuple :: Entry -> (String, Integer)
toTuple entry = (category entry, price entry)

parseEntries :: String -> [Entry]
parseEntries s = mapMaybe parseEntry (lines s)

parseEntry :: String -> Maybe Entry
parseEntry s =
  case splitOn "\t" s of
    [c, sv] -> Entry c <$> readMaybe sv
    _ -> Nothing

formatSummry :: Map String Integer -> String
formatSummry = unlines . map formatSummaryItem . Map.toList

formatSummaryItem :: (String, Integer) -> String
formatSummaryItem (cat, total) = cat ++ "\t" ++ show total
