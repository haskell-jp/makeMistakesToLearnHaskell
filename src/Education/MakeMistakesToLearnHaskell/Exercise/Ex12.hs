{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex12
  ( exercise12
  , generator
  , judge
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types


exercise12 :: Exercise
exercise12 = Exercise "12"
           $ runHaskellExerciseWithStdin diag generator judge


diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented


generator :: Gen Text
generator = do
  inputLines <- QuickCheck.listOf inputLine
  lastLine <- QuickCheck.oneof [pure "", category, withExtraField]
  return . Text.unlines $ inputLines ++ [lastLine]
 where
  inputLine = do
    cat <- category
    price <- Text.pack . show . QuickCheck.getPositive <$> (arbitrary :: Gen (QuickCheck.Positive Integer))
    separator <- QuickCheck.listOf1 $ pure ' '
    return $ cat <> Text.pack separator <> price

  category = fmap Text.pack . QuickCheck.listOf1 $ QuickCheck.choose ('A', 'z')

  withExtraField = do
    ln <- inputLine
    cat <- category
    return $ ln <> " " <> cat


judge :: Judge
judge input exitCode actualOut =
  case (exitCode, answer input) of
      (ExitSuccess, Right expectedOut) -> (expectedOut, actualOut == expectedOut)
      (ExitFailure _, Left expectedOut) -> (expectedOut, expectedOut `Text.isInfixOf` actualOut)
      (_, Right expectedOut) -> (expectedOut, False)
      (_, Left expectedOut) -> (expectedOut, False)

 where
  answer :: Text -> Either Text Text
  answer =
    bimap (<> "\n") ((<> "\n") . Text.pack . show)
      . sumEntries 0
      . Text.lines
   where
    sumEntries :: Integer -> [Text] -> Either Text Integer
    sumEntries currentSum lns =
      case lns of
          line : leftLines ->
            case Text.words line of
                [_cat, priceStr] ->
                  sumEntries (currentSum + read (Text.unpack priceStr)) leftLines
                [] ->
                  Right currentSum
                _ ->
                  Left ("Invalid input: " <> line)
          _ ->
            error "Assertion failure: empty input!"
