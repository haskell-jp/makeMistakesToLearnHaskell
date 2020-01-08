{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex11
  ( exercise11
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types


exercise11 :: Exercise
exercise11 = Exercise "11" notYetImplementedVeirificationExercise


{-
diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented


generator :: Gen String
generator = do
  input1 <- QuickCheck.frequency 
    [ ( 1
      , QuickCheck.listOf $ show . QuickCheck.getPositive <$> (arbitrary :: Gen (QuickCheck.Positive Double))
    ),( 1
      , QuickCheck.listOf1 $ show . QuickCheck.getPositive <$> (arbitrary :: Gen (QuickCheck.Positive Double))
    )]
  case input1 of
      [] -> (<>) "\n" <$> generator
      [height] -> (<>) (height <> "\n") <$> genWeight
      _  -> return $ unwords input1 <> "\n"

genWeight :: Gen String
genWeight = do
  input1 <- QuickCheck.frequency 
      [ ( 1
        , QuickCheck.listOf $ show . QuickCheck.getPositive <$> (arbitrary :: Gen (QuickCheck.Positive Double))
      ),( 1
        , QuickCheck.listOf1 $ show . QuickCheck.getPositive <$> (arbitrary :: Gen (QuickCheck.Positive Double))
      )]
  case input1 of
      [] -> (<>) "\n" <$> genWeight
      _ ->  return $ unwords input1 <> "\n"


answer :: Text -> Text
answer input = Text.pack $ show (body :: Double) <> "\n"
  where
    [principal, interestRate, years] = lines $ Text.unpack input
    body = read principal * (1 + read interestRate / 100) ^ (read years :: Integer)
-}
