{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise2Spec
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
  let subject = Exercise.unsafeGetById 2

  it "given the correct answer, show SUCCESS" $ do
    out <- ByteString.readFile "test/assets/2/error-messages/correct.txt"
    let e = setRunHaskellSuccessWithStdout baseEnv out
    void $ shouldSuccess =<< Exercise.verify subject e "assets/2.hs"

  itShouldFailForCaseWithMessage
    "2"
    "no-number-1"
    ["HINT: you might have forgot to write some numbers between operators ('*', '/' etc.)."]

  itShouldFailForCaseWithMessage
    "2"
    "no-number-2"
    ["HINT: you might have forgot to write some numbers between operators ('*', '/' etc.)."]

  itShouldFailForCaseWithMessage
    "2"
    "no-number-3"
    ["HINT: you might have forgot to write some numbers between operators ('*', '/' etc.)."]

  itShouldFailForCaseWithMessage
    "2"
    "no-paren"
    ["HINT: you might have forgot to write parentheses"]

  itShouldFailForCaseWithMessage
    "2"
    "no-close-paren"
    ["HINT: you might have forgot to write close parenthesis"]

  itShouldFailForCaseWithMessage
    "2"
    "no-main"
    ["HINT: This error indicates you haven't defined main function."]

  itShouldFailForCaseWithMessage
    "2"
    "no-open-paren"
    ["HINT: you might have forgot to write open parenthesis"]

  itShouldFailForCaseWithMessage
    "2"
    "no-slash"
    ["HINT: you might have forgot to write division operator '/'"]

  itShouldFailForCaseWithMessage
    "2"
    "no-star"
    ["HINT: you might have forgot to write multiplication operator '*'"]

  itShouldFailForCaseWithMessage "2" "typo" []

  it "given an answer printing wrong result, show FAIL" $ do
    err <- ByteString.readFile "test/assets/2/error-messages/wrong-number.txt"
    let e = setRunHaskellSuccessWithStdout baseEnv err
    d <- shouldFail =<< Exercise.verify subject e "test/assets/2/wrong-number.hs"
    d `shouldSatisfy` Text.isInfixOf "Your program's output: \"6.0\\n\""
    d `shouldSatisfy` Text.isInfixOf "Expected output: \"20.761245674740486\\n\""
