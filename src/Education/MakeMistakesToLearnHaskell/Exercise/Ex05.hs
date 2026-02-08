{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex05
  ( exercise5
  , answer
  , stdinGenerator
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types


exercise5 :: Exercise
exercise5 = Exercise "5"
          $ runHaskellExerciseWithStdinEq diag answer stdinGenerator


diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented


stdinGenerator :: Gen Text
stdinGenerator =
  Text.unlines <$> sequence
    [ Text.pack . show <$> (arbitrary :: Gen Double)
    , Text.pack . show <$> (arbitrary :: Gen Double)
    , Text.pack . show . QuickCheck.getPositive <$> (arbitrary :: Gen (QuickCheck.Positive Integer))
    ]


answer :: Text -> Text
answer input = Text.pack $ show (body :: Double) <> "\n"
  where
    (principal, interestRate, years) = assertTuple3 . lines $ Text.unpack input
    body = read principal * (1 + read interestRate / 100) ^ (read years :: Integer)

    assertTuple3 [l1, l2, l3] = (l1, l2, l3)
    assertTuple3 _ = error "Assertion failed: generated input must contain three lines"
