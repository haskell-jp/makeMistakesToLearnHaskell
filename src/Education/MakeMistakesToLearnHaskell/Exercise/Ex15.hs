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


argsGenerator :: Gen [CommandLineArg]
argsGenerator = QuickCheck.listOf $
  FilePath <$> filePath <*> fileContent
 where
  -- NOTE: On Windows, passing an empty string in command line arguments is hard.
  filePath =
    QuickCheck.listOf1
      . QuickCheck.elements
      $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']
  fileContent =
    fmap Text.pack
      . QuickCheck.listOf1
      . QuickCheck.elements
      $ ['A'..'z'] ++ ['0'..'9'] ++ " \t\n"


answer :: [CommandLineArg] -> Text
answer = Text.concat . map readAndIndent
 where
  readAndIndent (FilePath _path content) =
    let indentedLines = map (\l -> "  " <> l) (Text.lines content)
     in Text.unlines indentedLines
  readAndIndent (Mere string) =
    error $ "Assertion failure: command line argument without file content: " ++ show string
