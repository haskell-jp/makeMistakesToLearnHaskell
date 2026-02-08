{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex09
  ( exercise9
  , stdinGenerator
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types


exercise9 :: Exercise
exercise9 = Exercise "9"
          $ runHaskellExerciseWithStdinEq diag answer stdinGenerator


diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented


-- * Generate two numbers, seprated by either a newline or spaces.
-- * Or generate only a blank string as an error case.
--   * Don't generate only one number case: the `read` function throws an error,
--     which is not considered in this exercise.
stdinGenerator :: Gen Text
stdinGenerator = QuickCheck.oneof [twoNumbers, noNumbers]
 where
  twoNumbers = do
    height <- Text.pack . show . QuickCheck.getPositive <$> (arbitrary :: Gen (QuickCheck.Positive Double))
    weight <- Text.pack . show . QuickCheck.getPositive <$> (arbitrary :: Gen (QuickCheck.Positive Double))
    separator <- Text.pack <$> QuickCheck.oneof [pure "\n", QuickCheck.listOf1 (pure ' ')]
    return $ height <> separator <> weight <> "\n"
  noNumbers = (<> "\n") . Text.pack <$> (QuickCheck.listOf (pure ' '))


answer :: Text -> Text
answer input = Text.pack $ "Height Weight: \n" <> a <>"\n"
 where
  a = case lines $ Text.unpack input of
    [""] -> "Invalid input"
    [line1] -> show (weight / (height * height))
      where
        height, weight :: Double
        (height, weight) =
          case words line1 of
            heightStr : weightStr : _ ->
              (read heightStr, read weightStr)
            _ ->
              error "Assertion failed: generated line must contain two numbers"
    line1 : line2 : _ -> "Weight: \n" <> show (weight / (height * height))
      where height, weight :: Double
            height = read line1
            weight = read line2
    [] -> error "Assertion failure: empty input!"
