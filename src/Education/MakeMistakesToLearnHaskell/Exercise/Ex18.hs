{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex18
  ( exercise18
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types

import qualified Education.MakeMistakesToLearnHaskell.Exercise.Ex16 as Ex16


exercise18 :: Exercise
exercise18 = Exercise "18"
           $ runHaskellExerciseWithArgsEq diag Ex16.answer Ex16.argsGenerator


diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented
