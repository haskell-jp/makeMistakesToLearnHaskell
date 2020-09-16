{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex16
  ( exercise16
  , argsGenerator
  , answer
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.CommandLineArg
import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types


exercise16 :: Exercise
exercise16 = Exercise "16"
           $ runHaskellExerciseWithArgsEq diag answer argsGenerator


diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented


argsGenerator :: Gen [CommandLineArg]
argsGenerator = fmap (List.nubBy ((==) `on` assertFilePath)) . QuickCheck.listOf $
  FilePath <$> filePath <*> fileContent
 where
  -- NOTE: On Windows, passing an empty string in command line arguments is hard.
  filePath =
    QuickCheck.listOf1
      . QuickCheck.elements
      $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

  fileContent =
    fmap Text.unwords . QuickCheck.shuffle . concat =<< QuickCheck.listOf wordGroup

  wordGroup :: Gen [Text]
  wordGroup = do
    generatedWord <- word
    count <- QuickCheck.choose (1, 5)
    return $ replicate count generatedWord

  word :: Gen Text
  word =
    fmap Text.pack
      . QuickCheck.listOf1
      . QuickCheck.elements
      $ ['A'..'z'] ++ ['0'..'9']


answer :: [CommandLineArg] -> Text
answer =
  Text.unlines
    . map (\(word, count) -> word <> " => " <> Text.pack (show count))
    . Map.toList
    . Map.fromListWith (+)
    . map (\w -> (w, 1 :: Integer))
    . concatMap readWords
 where
  readWords (FilePath _path content) = Text.words content
  readWords (Mere string) =
    error $ "Assertion failure: command line argument without file content: " ++ show string
