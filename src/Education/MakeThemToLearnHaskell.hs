{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeThemToLearnHaskell
  ( main
  ) where


#include <imports/external.hs>

import qualified Education.MakeThemToLearnHaskell.Exercise as Exercise
import           Education.MakeThemToLearnHaskell.Util


main :: IO ()
main = do
  args <- Env.getArgs
  case args of
      ("verify" : left) -> verifySource left
      ("show" : left) -> showExercise left
      _ -> printExerciseList


printExerciseList :: IO ()
printExerciseList = do
  Text.putStrLn "# Make Them to Learn Haskell!"
  Text.putStrLn ""
  Text.putStrLn "## Contents"

  let printHeader n h = Text.putStrLn $ Text.pack (show n) <> ". " <> h
  zipWithM_ printHeader ([1..] :: [Int]) =<< Exercise.loadHeaders

  Text.putStrLn $ "\nRun `" <> Text.pack appName <> " show <the exercise number>` to try the exercise."


verifySource :: [FilePath] -> IO ()
verifySource [] = die "Specify the Haskell source file to veirfy!"
verifySource (file : _) = do
  currentExercise <- Exercise.loadLastShown
  result <- Exercise.verify currentExercise file
  case result of
      Exercise.Success details -> do
        Text.putStrLn details
        putStrLn "\n\nSUCCESS: Congratulations! Your solution got compiled and ran correctly!"
        putStrLn "Here's an example solution of this exercise:"
        Text.putStr =<< Exercise.loadExampleSolution currentExercise
        Exit.exitSuccess
      Exercise.Fail details -> do
        Error.errLn $ Text.unpack $ details <> "\n\n"
        Exit.die "FAIL: Your solution didn't pass. Try again!"
      Exercise.Error details -> do
        Error.errLn $ Text.unpack $ details <> "\n\n"
        die "An unexpected error occurred when evaluating your solution."


showExercise :: [String] -> IO ()
showExercise [] = die "Specify an exercise number to show"
showExercise (nStr : _) = do
  n <- dieWhenNothing ("Exercise id " ++ nStr ++ " not found!") (readMay nStr)
  d <- Exercise.loadDescriptionById n >>= dieWhenNothing ("Invalid exercise id: '" ++ nStr ++ "'")
  Exercise.saveLastShownId n
  Text.putStr d
