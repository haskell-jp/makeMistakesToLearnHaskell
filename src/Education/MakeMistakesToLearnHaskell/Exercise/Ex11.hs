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
answer input = "Height Weight: \n" <> go (Text.lines input) <> "\n"
 where
  go lns = case lns of
    "" : leftLines ->
      "Invalid input" <> go leftLines
    [line1] -> -- TODO: Keep asking weight
      Text.pack . show $ bmiFromStrings heightStr weightStr
      where heightStr : weightStr : _ = Text.words line1
    line1 : line2 : _ -> "Weight: \n" <> Text.pack (show $ bmiFromStrings line1 line2)
    [] ->
      error "Assertion failure: empty input!"

bmiFromStrings :: Text -> Text -> Double
bmiFromStrings hwStr weightStr = do
  let height = read $ Text.unpack hwStr
      weight = read $ Text.unpack weightStr
  weight / (height * height)


askWeight = do
  putStrLn "Weight: "
  ans <- getLine
  case words ans of
      weightStr : _ ->
        return weightStr
      _ -> do
        putStrLn ("Invalid input: " ++ ans)
        askWeight
