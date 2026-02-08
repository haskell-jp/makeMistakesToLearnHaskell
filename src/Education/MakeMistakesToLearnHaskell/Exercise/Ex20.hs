{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex20
  ( exercise20
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types

import qualified Education.MakeMistakesToLearnHaskell.Exercise.Ex05 as Ex05


exercise20 :: Exercise
exercise20 = Exercise "20"
           $ runHaskellExerciseWithStdinEq diag Ex05.answer Ex05.stdinGenerator


diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented
