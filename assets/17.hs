import Text.Read (readMaybe)
import qualified Data.Map.Strict as M

main = go M.empty
 where
  go m = do
    putStrLn "Enter command:"
    command <- getLine
    case parseCommand command of
        Just (Add k v) -> do
          putStrLn ("Adding " ++ show v ++ " to " ++ show k ++ ".")
          go (M.insertWith (\v1 v2 -> v1 + v2) k v m)
        Just (Get k) -> do
          case M.lookup k m of
              Just v ->
                putStrLn (k ++ " => " ++ show v)
              _ ->
                putStrLn "Error: no item found"
          go m
        Just Quit ->
          putStrLn "Bye."
        Nothing -> do
          putStrLn "Error: Invalid Command"
          go m

data Command =
    Add String Integer
  | Get String
  | Quit

parseCommand :: String -> Maybe Command
parseCommand s =
  case words s of
      ["add", k, mv] ->
        case readMaybe mv of
            Just v -> Just (Add k v)
            Nothing -> Nothing
      ["get", k] -> Just (Get k)
      ["quit"] -> Just Quit
      _ -> Nothing
