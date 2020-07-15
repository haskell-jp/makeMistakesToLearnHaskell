{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex12
  ( exercise12
  , stdinGenerator
  , stdinGeneratorOfSeparator
  , judge
  , judgeWith
  , answerWith
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types


exercise12 :: Exercise
exercise12 = Exercise "12"
           $ runHaskellExerciseWithStdin diag judge stdinGenerator


diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented

stdinGeneratorOfSeparator :: Char -> Gen Text
stdinGeneratorOfSeparator sepChar = do
  inputLines <- QuickCheck.listOf inputLine
  -- lastLine <- QuickCheck.oneof [pure "", category, withExtraField]
  lastLine <- QuickCheck.oneof [category, withExtraField]
  return . Text.unlines $ inputLines ++ [lastLine]
 where
  inputLine = do
    cat <- category
    price <- Text.pack . show . QuickCheck.getPositive <$> (arbitrary :: Gen (QuickCheck.Positive Integer))
    separator <- QuickCheck.listOf1 $ pure sepChar
    return $ cat <> Text.pack separator <> price

  category = fmap Text.pack . QuickCheck.listOf1 $ QuickCheck.choose ('A', 'z')

  withExtraField = do
    ln <- inputLine
    cat <- category
    return $ ln <> Text.pack [sepChar] <> cat


stdinGenerator :: Gen Text
stdinGenerator = stdinGeneratorOfSeparator ' '


judge :: Judge
judge = judgeWith Text.words


judgeWith :: (Text -> [Text]) -> Judge
judgeWith split _args input exitCode actualOut =
  case (exitCode, answerWith split input) of
      (ExitSuccess, Right expectedOut) -> (expectedOut, actualOut == expectedOut)
      (ExitFailure _, Left expectedOut) -> (expectedOut, expectedOut `Text.isInfixOf` actualOut)
      (_, Right expectedOut) -> (expectedOut, False)
      (_, Left expectedOut) -> (expectedOut, False)


answerWith :: (Text -> [Text]) -> Text -> Either Text Text
answerWith split =
  bimap (<> "\n") ((<> "\n") . Text.pack . show)
    . sumEntries 0
    . Text.lines
 where
  -- NOTE: To avoid space-leak (at least in the judge function), use BangPatterns
  sumEntries :: Integer -> [Text] -> Either Text Integer
  sumEntries !currentSum lns =
    case lns of
        line : leftLines ->
          case split line of
              [_cat, priceStr] ->
                sumEntries (currentSum + read (Text.unpack priceStr)) leftLines
              [] ->
                Right currentSum
              _ ->
                Left ("Invalid input: " <> line)
        _ ->
          error "Assertion failure: empty input!"
