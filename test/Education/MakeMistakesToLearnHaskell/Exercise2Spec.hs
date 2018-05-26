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
spec =
  describe "Education.MakeMistakesToLearnHaskell.Exercise 2" $ do
    baseEnv <- mkDefaultSpecEnv
    let subject = Exercise.unsafeGetById 2

    it "given an answer missing close parenthesis, show FAIL" $ do
      err <- ByteString.readFile "test/assets/2/error-messages/no-close-paren.txt"
      let e = setRunHaskellReturningOutput baseEnv err
      d <- shouldFail =<< Exercise.verify subject e "test/assets/2/no-close-paren.hs"
      d `shouldSatisfy` Text.isInfixOf "HINT: you might have forgot to write close parenthesis"

    it "given an answer without main, show FAIL" $ do
      err <- ByteString.readFile "test/assets/2/error-messages/no-main.txt"
      let e = setRunHaskellReturningOutput baseEnv err
      d <- shouldFail =<< Exercise.verify subject e "test/assets/2/no-main.hs"
      d `shouldSatisfy` Text.isInfixOf "HINT: This error indicates you haven't defined main function, or misspelled 'main'."

    it "given an answer missing open parenthesis, show FAIL" $ do
      err <- ByteString.readFile "test/assets/2/error-messages/no-open-paren.txt"
      let e = setRunHaskellReturningOutput baseEnv err
      d <- shouldFail =<< Exercise.verify subject e "test/assets/2/no-open-paren.hs"
      d `shouldSatisfy` Text.isInfixOf "HINT: you might have forgot to write open parenthesis"

    it "given an answer missing division operator, show FAIL" $ do
      err <- ByteString.readFile "test/assets/2/error-messages/no-slash.txt"
      let e = setRunHaskellReturningOutput baseEnv err
      d <- shouldFail =<< Exercise.verify subject e "test/assets/2/no-slash.hs"
      d `shouldSatisfy` Text.isInfixOf "HINT: you might have forgot to write division operator '/'"

    it "given an answer missing multiplication operator, show FAIL" $ do
      err <- ByteString.readFile "test/assets/2/error-messages/no-star.txt"
      let e = setRunHaskellReturningOutput baseEnv err
      d <- shouldFail =<< Exercise.verify subject e "test/assets/2/no-star.hs"
      d `shouldSatisfy` Text.isInfixOf "HINT: you might have forgot to write multiplication operator '*'"

    it "given an answer with typo, show FAIL" $ do
      err <- ByteString.readFile "test/assets/2/error-messages/typo.txt"
      let e = setRunHaskellReturningOutput baseEnv err
      void (shouldFail =<< Exercise.verify subject e "test/assets/2/typo.hs")

    it "given an answer printing wrong result, show FAIL" $ do
      err <- ByteString.readFile "test/assets/2/error-messages/wrong-number.txt"
      let e = setRunHaskellReturningOutput baseEnv err
      d <- shouldFail =<< Exercise.verify subject e "test/assets/2/wrong-number.hs"
      d `shouldSatisfy` Text.isInfixOf "Your program's output: \"6.0\n\""
      d `shouldSatisfy` Text.isInfixOf "Expected output: \"20.761245674740486\n\""
