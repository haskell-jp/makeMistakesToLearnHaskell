{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise3Spec
  ( spec
  , main
  ) where

#include <test/imports/external.hs>

import           Education.MakeMistakesToLearnHaskell.Env
import           Education.MakeMistakesToLearnHaskell.SpecEnv
import           Education.MakeMistakesToLearnHaskell.SpecHelper

import qualified Education.MakeMistakesToLearnHaskell.Exercise as Exercise


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  baseEnv <- mkDefaultSpecEnv
  let subject = Exercise.unsafeGetByName "3"

  it "given the correct answer, show SUCCESS" $ do
    out <- ByteString.readFile "test/assets/3/error-messages/correct.txt"
    let e = setRunHaskellSuccessWithStdout baseEnv out
    void $ shouldSuccess =<< Exercise.verify subject e "assets/3.hs"

  itShouldFailForCaseWithMessage
    "3"
    "no-main"
    ["HINT: This error indicates you haven't defined main function."]

  itShouldFailForCaseWithMessage
    "3"
    "incornsistent-indent1"
    ["HINT: instructions in a `do` must be in a consistent width."]

  itShouldFailForCaseWithMessage
    "3"
    "incornsistent-indent2"
    ["HINT: instructions in a `do` must be in a consistent width."]

  itShouldFailForCaseWithMessage
    "3"
    "no-do"
    ["HINT: You seem to forget to write `do`. `do` must be put before listing `putStrLn`s."]

  itShouldFailForCaseWithMessage
    "3"
    "single-quote"
    ["HINT: In Haskell, you must surround string literals with double-quote '\"'. Such as \"Hello, world\"."]

  itShouldFailForCaseWithMessage "3" "typo" []

  it "given an answer printing wrong result, show FAIL" $ do
    err <- ByteString.readFile "test/assets/3/error-messages/wrong-output.txt"
    let e = setRunHaskellSuccessWithStdout baseEnv err
    d <- shouldFail =<< Exercise.verify subject e "test/assets/3/wrong-output.hs"
    d `shouldSatisfy` Text.isInfixOf "Your program's output:" -- TODO: better output
    d `shouldSatisfy` Text.isInfixOf "Expected output:"
