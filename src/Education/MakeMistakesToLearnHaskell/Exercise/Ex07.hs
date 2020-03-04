{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex07
  ( exercise7
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Ex06 (generator)
import Education.MakeMistakesToLearnHaskell.Exercise.Types


exercise7 :: Exercise
exercise7 = Exercise "7"
          $ runHaskellExerciseWithStdinEq diag generator answer


diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented

data Entry = Entry
  { category :: String
  , price :: Integer
  } deriving Show

answer :: Text -> Text
answer input = Text.pack $ show (Entry cat (read priceStr)) <> "\n"
 where
  [cat, priceStr] = lines $ Text.unpack input
