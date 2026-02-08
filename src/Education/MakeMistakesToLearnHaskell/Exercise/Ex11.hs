{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex11
  ( exercise11
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types


exercise11 :: Exercise
exercise11 = Exercise "11"
          $ runHaskellExerciseWithStdinEq diag answer stdinGenerator


diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented


stdinGenerator :: Gen Text
stdinGenerator = (<>) <$> noNumbers <*> twoNumbers
 where
  twoNumbers = do
    height <- Text.pack . show . QuickCheck.getPositive <$> (arbitrary :: Gen (QuickCheck.Positive Double))
    weight <- Text.pack . show . QuickCheck.getPositive <$> (arbitrary :: Gen (QuickCheck.Positive Double))
    separator <- QuickCheck.listOf1 (QuickCheck.elements " \n")
    return $ height <> Text.pack separator <> weight <> "\n"
  noNumbers = Text.pack <$> QuickCheck.listOf (QuickCheck.elements " \n")


answer :: Text -> Text
answer input = go (Text.lines input) <> "\n"
 where
  go lns = "Height Weight: \n" <> result
   where
    result =
      case lns of
          line : leftLines ->
            case Text.words line of
                heightStr : weightStr : _ ->
                  Text.pack . show $ bmiFromStrings heightStr weightStr
                [heightStr] ->
                  goWeight leftLines heightStr
                [] ->
                  "Invalid input\n" <> go leftLines
          [] ->
            error "Assertion failure: empty input!"

  goWeight lns heightStr = "Weight: \n" <> result
   where
    result =
      case lns of
          line : leftLines ->
            case Text.words line of
                weightStr : _ ->
                  Text.pack . show $ bmiFromStrings heightStr weightStr
                [] ->
                  "Invalid input\n" <> goWeight leftLines heightStr
          [] ->
            error "Assertion failure: empty input!"

bmiFromStrings :: Text -> Text -> Double
bmiFromStrings hwStr weightStr = do
  let height = read $ Text.unpack hwStr
      weight = read $ Text.unpack weightStr
  weight / (height * height)
