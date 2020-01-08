{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex10
  ( exercise10
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types


exercise10 :: Exercise
exercise10 = Exercise "10"
          $ runHaskellExerciseWithStdin diag generator answer


diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented


generator :: Gen String
generator = do
  input1 <- QuickCheck.listOf $ show . QuickCheck.getPositive <$> (arbitrary :: Gen (QuickCheck.Positive Double))
  case input1 of
      [height] -> do
          input2 <- show . QuickCheck.getPositive <$> (arbitrary :: Gen (QuickCheck.Positive Double))
          return $ unlines [height, input2]
      _ -> return $ unwords input1 <> "\n"


answer :: Text -> Text
answer input = Text.pack $ "Height Weight: \n" <> a <>"\n"
 where
  a = case lines $ Text.unpack input of
    [""] -> "Invalid input: "
    [line1] -> show (bmiFromStrings heightStr weightStr)
      where heightStr : weightStr : _ = words line1
    line1 : line2 : _ -> "Weight: \n" <> show (bmiFromStrings line1 line2)
    [] -> error "Assertion failure: empty input!"

bmiFromStrings :: String -> String -> Double
bmiFromStrings hwStr weightStr = weight / (height * height)
 where
  height = read hwStr
  weight = read weightStr
