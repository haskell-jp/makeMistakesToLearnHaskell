{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex05
  ( exercise5
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types

data Exercise5 = Exercise5 Double Double (Positive Int)
  deriving Show

instance Arbitrary Exercise5 where
  arbitrary = Exercise5
           <$> (arbitrary :: Gen Double)
           <*> (arbitrary :: Gen Double)
           <*> (arbitrary :: Gen (Positive Int))

exercise5 :: Exercise
exercise5 = Exercise "5"
          $ runHaskellExerciseWithStdin diag generator answer

diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented

generator :: (Gen Exercise5, Exercise5 -> [String])
generator = (arbitrary, gen2string)

answer :: Text -> Text
answer input = Text.pack $ show (body :: Double) <> "\n"
  where
    [principal, interestRate, years] = lines $ Text.unpack input
    body = read principal * (1 + read interestRate / 100) ^ (read years :: Integer)

gen2string :: Exercise5 -> [String]
gen2string (Exercise5 a b c) =
  [ show a
  , show b
  , show $ QuickCheck.getPositive c
  ]
