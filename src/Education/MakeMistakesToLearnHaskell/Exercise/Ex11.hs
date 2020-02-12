{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex11
  ( exercise11
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types


exercise11 :: Exercise
exercise11 = Exercise "11"
          $ runHaskellExerciseWithStdin diag generator answer


diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented


generator :: Gen String
generator = (++) <$> noNumbers <*> twoNumbers
 where
  twoNumbers = do
    height <- show . QuickCheck.getPositive <$> (arbitrary :: Gen (QuickCheck.Positive Double))
    weight <- show . QuickCheck.getPositive <$> (arbitrary :: Gen (QuickCheck.Positive Double))
    separator <- QuickCheck.listOf1 (QuickCheck.elements " \n")
    return $ height ++ separator ++ weight ++ "\n"
  noNumbers = QuickCheck.listOf (QuickCheck.elements " \n")


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
