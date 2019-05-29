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

  let wordCounts =
        Map.toList (Map.fromListWith (\x y -> x + y) (map (\w -> (w, 1)) (concat wordss)))

  for_ (wordCounts) (\(word, count) ->
    putStrLn (word ++ " => " ++ show count)
    )
