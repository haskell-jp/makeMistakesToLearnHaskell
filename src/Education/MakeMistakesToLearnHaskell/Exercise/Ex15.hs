{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex15
  ( exercise15
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types


exercise15 :: Exercise
exercise15 = Exercise "15"
           $ runHaskellExerciseWithArgsEq diag answer argsGenerator


diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented


argsGenerator :: Gen [String]
argsGenerator = QuickCheck.listOf nonEmptyString
 where
  -- NOTE: On Windows, passing an empty string in command line arguments is hard.
  nonEmptyString =
    QuickCheck.listOf1
      . QuickCheck.elements
      $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']


judge :: Judge
judge args _input _exitCode actualOut =
  case (exitCode, answer args) of
      (ExitSuccess, Right expectedOut) -> (expectedOut, actualOut == expectedOut)
      (ExitFailure _, Left expectedOut) -> (expectedOut, expectedOut `Text.isInfixOf` actualOut)
      (_, Right expectedOut) -> (expectedOut, False)
      (_, Left expectedOut) -> (expectedOut, False)


answer :: [String] -> IO Text
answer args =
  for_ args (\arg -> do
    putStrLn arg
    content <- readFile arg -- TODO: Create temporary file for input with generated names
    let indentedLines = map (\l -> "  " <> l) (lines content)
    putStr (unlines indentedLines))
