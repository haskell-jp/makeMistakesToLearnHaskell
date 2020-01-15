{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex08
  ( exercise8
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types


exercise8 :: Exercise
exercise8 = Exercise "8"
          $ runHaskellExerciseWithStdin diag generator answer


diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented


generator :: Gen String
generator =
  unlines <$> sequence
    [ show <$> (arbitrary :: Gen Integer)
    , show . QuickCheck.getNonZero <$> (arbitrary :: Gen (QuickCheck.NonZero Integer))
    ]

answer :: Text -> Text
answer input = Text.pack
  $  "div: "  <> show divResult  <> "\n"
  <> "mod: "  <> show modResult  <> "\n"
  <> "quot: " <> show quotResult <> "\n"
  <> "rem: "  <> show remResult  <> "\n"
 where
  [numeratorStr, denominatorStr] = lines $ Text.unpack input
  numerator = read numeratorStr :: Integer
  denominator = read denominatorStr :: Integer
  (divResult, modResult) = divMod numerator denominator
  (quotResult, remResult) = quotRem numerator denominator
