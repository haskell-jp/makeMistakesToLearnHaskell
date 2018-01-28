{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeThemToLearnHaskell.Exercise1Spec
  ( spec
  , main
  ) where

#include <test/imports/external.hs>

import           Education.MakeThemToLearnHaskell.Env
import           Education.MakeThemToLearnHaskell.SpecEnv

import qualified Education.MakeThemToLearnHaskell.Exercise as Exercise


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Education.MakeThemToLearnHaskell.Exercise 1" $ do
    baseEnv <- mkDefaultSpecEnv
    let subject = Exercise.unsafeGetById 1

    it "given an answer whose 'Hello, world!' is singlequoted, show FAIL" $ do
      err <- ByteString.readFile "test/assets/1/error-messages/single-quote.txt"
      let e = setRunHaskellReturningOutput baseEnv err
      d <- shouldFail =<< Exercise.verify subject e "test/assets/1/single-quote.hs"
      d `shouldSatisfy` Text.isInfixOf "HINT: In Haskell, you must surround string literals with double-quote '\"'. Such as \"Hello, world\"."

    it "given an answer with typo, show FAIL" $ do
      err <- ByteString.readFile "test/assets/1/error-messages/typo.txt"
      let e = setRunHaskellReturningOutput baseEnv err
      d <- shouldFail =<< Exercise.verify subject e "test/assets/1/typo.hs"
      d `shouldSatisfy` Text.isInfixOf "HINT: you might have misspelled 'putStrLn'."


shouldFail :: Exercise.Result -> IO Exercise.Details
shouldFail r =
  case r of
      Exercise.Fail d ->
        return d
      Exercise.Success d ->
        fail $ "Unexpected Success: " ++ show d
      Exercise.Error d ->
        fail $ "Unexpected Error: " ++ show d
