{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex17
  ( exercise17
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types


exercise17 :: Exercise
exercise17 = Exercise "17" notYetImplementedVeirificationExercise


{-
diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented


generator :: Gen String
generator =
  unlines <$> sequence
    [ show <$> (arbitrary :: Gen Double)
    , show <$> (arbitrary :: Gen Double)
    , show . QuickCheck.getPositive <$> (arbitrary :: Gen (QuickCheck.Positive Int))
    ]


answer :: Text -> Text
answer input = Text.pack $ show (body :: Double) <> "\n"
  where
    [principal, interestRate, years] = lines $ Text.unpack input
    body = read principal * (1 + read interestRate / 100) ^ (read years :: Integer)
-}