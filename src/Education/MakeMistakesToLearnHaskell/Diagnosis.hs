{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Diagnosis
  ( appendDiagnosis
  ) where


#include <imports/external.hs>

import           Education.MakeMistakesToLearnHaskell.Text
import           Education.MakeMistakesToLearnHaskell.Evaluator.Types
import           Education.MakeMistakesToLearnHaskell.Exercise.Types


appendDiagnosis :: Diagnosis -> SourceCode -> ErrorMessage -> Details
appendDiagnosis d c m =
  let m' = decodeUtf8 m
  in m' <> "\n" <> d c m'
