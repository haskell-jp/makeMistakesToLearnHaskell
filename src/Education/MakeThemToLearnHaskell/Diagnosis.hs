{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeThemToLearnHaskell.Diagnosis
  ( appendDiagnosis
  , diagnoseErrorMessage
  ) where


#include <imports/external.hs>

import           Education.MakeThemToLearnHaskell.Evaluator.Types
import           Education.MakeThemToLearnHaskell.Exercise.Types

import           Debug.Trace


appendDiagnosis :: ErrorMessage -> Details
appendDiagnosis m =
  let d = TextEncoding.decodeUtf8 m
  in
    d <> "\n" <> diagnoseErrorMessage d


diagnoseErrorMessage :: Details -> Details
diagnoseErrorMessage msg
  | "Variable not in scope: main :: IO" `Text.isInfixOf` msg =
    "HINT: This error indicates you haven't defined main function."
  | "Variable not in scope:" `Text.isInfixOf` msg =
    "HINT: you might have misspelled 'putStrLn'."
  | otherwise = ""
