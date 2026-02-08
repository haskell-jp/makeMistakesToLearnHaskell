{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex19
  ( exercise19
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types
import qualified Education.MakeMistakesToLearnHaskell.Exercise.Ex04 as Ex04


exercise19 :: Exercise
exercise19 = Exercise "19"
          $ runHaskellExerciseWithStdinEq diag Ex04.answer Ex04.stdinGen


diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented
