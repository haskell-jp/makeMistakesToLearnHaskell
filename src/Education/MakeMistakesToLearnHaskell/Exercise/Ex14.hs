{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex14
  ( exercise14
  , stdinGenerator
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types
import qualified Education.MakeMistakesToLearnHaskell.Exercise.Ex12 as Ex12


exercise14 :: Exercise
exercise14 = Exercise "14"
           $ runHaskellExerciseWithStdin diag judge stdinGenerator


diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented


stdinGenerator :: Gen Text
stdinGenerator = Ex12.stdinGeneratorOfSeparator '\t'


judge :: Judge
judge = Ex12.judgeWith (Text.splitOn "\t")
