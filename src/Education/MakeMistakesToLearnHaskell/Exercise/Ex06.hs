{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex06
  ( exercise6
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types


exercise6 :: Exercise
exercise6 = Exercise "6"
          $ runHaskellExerciseWithStdin diag generator answer


diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented


generator :: Gen String
generator =
  unlines <$> sequence
  [ QuickCheck.listOf $ QuickCheck.oneof [QuickCheck.choose ('a','z'),  QuickCheck.choose ('A','Z')]
  , show . QuickCheck.getPositive <$> (arbitrary :: Gen (QuickCheck.Positive Int))
  ]

data Entry = Entry
  { category :: String
  , price :: Integer
  }

answer :: Text -> Text
answer input = Text.pack $ (lineCategory :: String) <> "\n" <> (linePrice :: String) <> "\n"
 where
  [cat, priceStr] = lines $ Text.unpack input
  entry = Entry { category = cat, price = read priceStr }
  lineCategory = "Category: " <> category entry
  linePrice = "Price: " <> show (price entry)
