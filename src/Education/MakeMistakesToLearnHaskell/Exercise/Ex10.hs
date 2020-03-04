{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex10
  ( exercise10
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Ex09 (generator)
import Education.MakeMistakesToLearnHaskell.Exercise.Types


exercise10 :: Exercise
exercise10 = Exercise "10"
          $ runHaskellExerciseWithStdinEq diag generator answer


diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented


answer :: Text -> Text
answer input = Text.pack $ "Height Weight: \n" <> a <>"\n"
 where
  a = case Text.lines $ input of
    [""] -> "Invalid input"
    [line1] -> show (bmiFromStrings heightStr weightStr)
      where heightStr : weightStr : _ = Text.words line1
    line1 : line2 : _ -> "Weight: \n" <> show (bmiFromStrings line1 line2)
    [] -> error "Assertion failure: empty input!"

bmiFromStrings :: Text -> Text -> Double
bmiFromStrings hwStr weightStr = do
  let height = read $ Text.unpack hwStr
      weight = read $ Text.unpack weightStr
  weight / (height * height)
