{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex12
  ( exercise12
  , generator
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types


exercise12 :: Exercise
exercise12 = Exercise "12"
           $ runHaskellExerciseWithStdinEq diag generator answer


diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented


generator :: Gen Text
generator = do
  inputLines <- QuickCheck.listOf inputLine
  return . Text.unlines $ inputLines ++ [""]
 where
  inputLine = do
    cat <- fmap Text.pack . QuickCheck.listOf1 $ QuickCheck.choose ('A', 'z')
    price <- Text.pack . show . QuickCheck.getPositive <$> (arbitrary :: Gen (QuickCheck.Positive Integer))
    separator <- QuickCheck.listOf1 $ pure ' '
    return $ cat <> Text.pack separator <> price


answer :: Text -> Text
answer = undefined
