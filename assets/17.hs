import Text.Read (readMaybe)
import qualified Data.Map.Strict as M

main = go M.empty
 where
  go m = do
    putStrLn "Enter command:"
    command <- getLine
    case words command of
        ["add", k, mv] -> do
          case readMaybe mv of
            Just v -> do
              putStrLn ("Adding " ++ show v ++ " to " ++ show k ++ ".")
              go (M.insertWith (\v1 v2 -> v1 + v2) k v m)
            Nothing -> do
              putStrLn "Error: Invalid Command"
              go m
        ["get", k] -> do
          case M.lookup k m of
              Just v ->
                putStrLn (k ++ " => " ++ show v)
              _ ->
                putStrLn "Error: no item found"
          go m
        ["quit"] ->
          putStrLn "Bye."
        _ -> do
          putStrLn "Error: Invalid Command"
          go m
