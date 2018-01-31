import           System.Environment (getArgs)
import           System.IO (stdout, hSetBuffering, BufferMode(..))
import           Text.Read (readMaybe)

-- TODO: 短くした例も書く

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case args of
      (first : _) -> do
        let maybeAnswer = readMaybe first
        case maybeAnswer of
            Just answer -> do
              gameLoop answer
            Nothing -> do
              error ("ERROR: '" ++ show first ++ "' is not a number!")
      [] -> do
        error "Give answer"


gameLoop :: Int -> IO ()
gameLoop answer = do
  putStrLn "Enter an answer"
  line <- getLine
  let maybePlayerAnswer = readMaybe line
  case maybePlayerAnswer of
      Just playerAnswer -> do
        if playerAnswer == answer then do
          putStrLn "Right!"
        else if playerAnswer < answer then do
          putStrLn "Too small!"
          putStrLn "Try once more!"
          gameLoop answer
        else do
          putStrLn "Too large!"
          putStrLn "Try once more!"
          gameLoop answer
      Nothing -> do
        putStrLn ("ERROR: " ++ show line ++ " is not a number!")
        putStrLn "Try once more!"
        gameLoop answer
