import Data.Traversable
import Data.Foldable
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.Environment

main = do
  files <- getArgs
  wordss <- for files (\file -> do
    contents <- readFile file
    return (words contents)
    )

  let words = concat wordss
      wordAnd1s = map (\w -> (w, 1)) words
      countsByWord =
        Map.fromListWith (\x y -> x + y) wordAnd1s
      wordCounts = Map.toList countsByWord

  for_ wordCounts (\wordCount ->
    putStrLn (fst wordCount ++ " => " ++ show (snd wordCount))
    )
