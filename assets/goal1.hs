import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Traversable (for)
import System.Environment (getArgs)
import System.IO (BufferMode(..), hSetBuffering, stdout)

data Entry =
  Entry { category :: String, price :: Integer } deriving Show

main = do
  files <- getArgs
  entries <-
    for files (\path -> do
      contents <- readFile path
      return (map toTuple (parseEntries contents))
    )
  let summary = Map.fromListWith (\x y -> x + y) (concat entries)
  putStr (formatSummary summary)

toTuple :: Entry -> (String, Integer)
toTuple entry = (category entry, price entry)

parseEntries :: String -> [Entry]
parseEntries s = map parseEntry (lines s)

parseEntry :: String -> Entry
parseEntry s =
  case splitOn "\t" s of
    [c, sv] -> Entry c (read sv)
    _ -> error ("Invalid entry: " ++ show s)

formatSummary :: Map String Integer -> String
formatSummary summary = unlines (map formatSummaryItem (Map.toList summary))

formatSummaryItem :: (String, Integer) -> String
formatSummaryItem catTotal = fst catTotal ++ "\t" ++ show (snd catTotal)
