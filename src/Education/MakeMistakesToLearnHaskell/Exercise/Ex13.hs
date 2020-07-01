{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex13
  ( exercise13
  , argsGenerator
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types


exercise13 :: Exercise
exercise13 = Exercise "13"
           $ runHaskellExerciseWithArgsAndStdin diag judge argsGenerator stdinGenerator


diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented


argsGenerator :: Gen [String]
argsGenerator = QuickCheck.frequency [(1, args1), (1, args2), (3, doubles3), (1, argsMore)]
 where
  -- NOTE: On Windows, passing an empty string in command line arguments is hard.
  nonEmptyString =
    QuickCheck.listOf1
      . QuickCheck.elements
      $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

  args1 = (: []) <$> nonEmptyString
  args2 = QuickCheck.vectorOf 2 nonEmptyString
  doubles3 =
    QuickCheck.vectorOf 3 $ fmap (show . QuickCheck.getPositive)
    (QuickCheck.arbitrary :: QuickCheck.Gen (QuickCheck.Positive Double))
  argsMore = do
    args4 <- QuickCheck.vectorOf 4 nonEmptyString
    leftArgs <- QuickCheck.listOf nonEmptyString
    return $ args4 ++ leftArgs


stdinGenerator :: Gen Text
stdinGenerator = pure ""


judge :: Judge
judge args _input exitCode actualOut =
  case (exitCode, answer args) of
      (ExitSuccess, Right expectedOut) -> (expectedOut, actualOut == expectedOut)
      (ExitFailure _, Left expectedOut) -> (expectedOut, expectedOut `Text.isInfixOf` actualOut)
      (_, Right expectedOut) -> (expectedOut, False)
      (_, Left expectedOut) -> (expectedOut, False)


answer :: [String] -> Either Text Text
answer args =
  case args of
      [metsStr, weightStr, minutesStr] -> do
        let mets = read metsStr :: Double
            weight = read weightStr
            minutes = read minutesStr
        Right . Text.pack $ show (mets * weight * (minutes / 60) * 1.05) ++ "\n"
      _ ->
        Left . Text.pack $ "Invalid input: " ++ show args ++ "\n"
