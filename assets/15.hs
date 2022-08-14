import System.Environment (getArgs)
import Data.Foldable (for_)


main = do
  args <- getArgs
  for_ args (\arg -> do
    putStrLn arg
    content <- readFile arg
    let indentedLines = map (\l -> "  " ++ l) (lines content)
    putStr (unlines indentedLines)
    )
