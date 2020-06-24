{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex06
  ( exercise6
  , stdinGenerator
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types


exercise6 :: Exercise
exercise6 = Exercise "6"
          $ runHaskellExerciseWithStdinEq diag answer stdinGenerator


diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented


stdinGenerator :: Gen Text
stdinGenerator =
  Text.unlines <$> sequence
    [ fmap Text.pack . QuickCheck.listOf $ QuickCheck.choose ('A', 'z')
    , Text.pack . show . QuickCheck.getPositive <$> (arbitrary :: Gen (QuickCheck.Positive Integer))
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
