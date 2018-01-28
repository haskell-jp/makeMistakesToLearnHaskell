{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeThemToLearnHaskell.Diagnosis
  ( appendDiagnosis
  , diagnoseErrorMessage
  ) where


#include <imports/external.hs>

import           Education.MakeThemToLearnHaskell.Evaluator.Types
import           Education.MakeThemToLearnHaskell.Exercise.Types

import           Debug.Trace


appendDiagnosis :: SourceCode -> ErrorMessage -> Details
appendDiagnosis c m =
  let m' = TextEncoding.decodeUtf8 m
  in
    m' <> "\n" <> diagnoseErrorMessage c m'


diagnoseErrorMessage :: SourceCode -> Details -> Details
diagnoseErrorMessage code msg
  | "parse error on input" `Text.isInfixOf` msg
      && "'" `Text.isInfixOf` code =
        "HINT: In Haskell, you must surround string literals with double-quote '\"'. Such as \"Hello, world\"."
  | "Variable not in scope: main :: IO" `Text.isInfixOf` msg =
    "HINT: This error indicates you haven't defined main function."
  | "Variable not in scope:" `Text.isInfixOf` msg =
    "HINT: you might have misspelled 'putStrLn'."
  | otherwise = ""
