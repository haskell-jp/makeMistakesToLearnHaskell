{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeThemToLearnHaskell
  ( main
  ) where


#include <imports/external.hs>

import           Education.MakeThemToLearnHaskell.Env
import qualified Education.MakeThemToLearnHaskell.Exercise as Exercise
import qualified Education.MakeThemToLearnHaskell.Evaluator.RunHaskell as RunHaskell
import           Education.MakeThemToLearnHaskell.Error


main :: IO ()
main = do
  avoidCodingError
  args <- Env.getArgs
  withMainEnv $ \e ->
    case args of
        ("verify" : left) -> verifySource e left
        ("show" : left) -> showExercise e left
        _ -> printExerciseList


withMainEnv :: (Env -> IO r) -> IO r
withMainEnv action = do
  d <- Env.getEnv homePathEnvVarName <|> Dir.getXdgDirectory Dir.XdgData appName
  Dir.createDirectoryIfMissing True d
  IO.withFile (d </> "debug.log") IO.WriteMode $ \h ->
    action
      Env
        { logDebug = ByteString.hPutStr h . (<> "\n")
        , appHomePath = d
        , runHaskell = RunHaskell.runFile
        }


printExerciseList :: IO ()
printExerciseList = do
  Text.putStrLn "# Make Them to Learn Haskell!"
  Text.putStrLn ""
  Text.putStrLn "## Contents"

  let printHeader n h = Text.putStrLn $ Text.pack (show n) <> ". " <> h
  zipWithM_ printHeader ([1..] :: [Int]) =<< Exercise.loadHeaders

  Text.putStrLn $ "\nRun `" <> Text.pack appName <> " show <the exercise number>` to try the exercise."


verifySource :: Env -> [FilePath] -> IO ()
verifySource _ [] = die "Specify the Haskell source file to veirfy!"
verifySource e (file : _) = do
  currentExercise <- Exercise.loadLastShown e
  result <- Exercise.verify currentExercise e file
  case result of
      Exercise.Success details -> do
        Text.putStrLn details
        putStrLn "\n\nSUCCESS: Congratulations! Your solution got compiled and ran correctly!"
        putStrLn "Here's an example solution of this exercise:"
        Text.putStr =<< Exercise.loadExampleSolution currentExercise
        Exit.exitSuccess
      Exercise.Fail details -> do
        Text.putStrLn $ details <> "\n\nFAIL: Your solution didn't pass. Try again!"
        Exit.exitFailure
      Exercise.Error details -> do
        Error.errLn $ Text.toStrict $ details <> "\n\n"
        die "An unexpected error occurred when evaluating your solution."


showExercise :: Env -> [String] -> IO ()
showExercise _ [] = die "Specify an exercise number to show"
showExercise e (nStr : _) = do
  n <- dieWhenNothing ("Invalid exercise id: '" ++ nStr ++ "'") (readMay nStr)
  d <- Exercise.loadDescriptionById n
        >>= dieWhenNothing ("Exercise id " ++ nStr ++ " not found!")
  Exercise.saveLastShownId e n
  Text.putStr d
