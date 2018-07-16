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
  let subject = Exercise.unsafeGetById 4

  it "given the correct answer, show SUCCESS" $ do
    out <- ByteString.readFile "test/assets/4/error-messages/correct.txt"
    let e = setRunHaskellSuccessWithStdout baseEnv out
    void $ shouldSuccess =<< Exercise.verify subject e "assets/4.hs"

  it "given an answer with incornsistentindent, show FAIL" $ do
    err <- ByteString.readFile "test/assets/4/error-messages/incornsistent-indent.txt"
    let e = setRunHaskellFailureWithOutput baseEnv err
    d <- shouldFail =<< Exercise.verify subject e "test/assets/4/incornsistent-indent.hs"
    d `shouldSatisfy` Text.isInfixOf "HINT: instructions in a `do` must be in a consistent width. "

  it "given an answer without do, show FAIL" $ do
    err <- ByteString.readFile "test/assets/4/error-messages/no-do.txt"
    let e = setRunHaskellFailureWithOutput baseEnv err
    d <- shouldFail =<< Exercise.verify subject e "test/assets/4/no-do.hs"
    d `shouldSatisfy` Text.isInfixOf "HINT: You seem to forget to write `do`. `do` must be put before listing `putStrLn`s."

  it "given an answer whose 'Hello, world!' is singlequoted, show FAIL" $ do
    err <- ByteString.readFile "test/assets/4/error-messages/single-quote.txt"
    let e = setRunHaskellFailureWithOutput baseEnv err
    d <- shouldFail =<< Exercise.verify subject e "test/assets/4/single-quote.hs"
    d `shouldSatisfy` Text.isInfixOf "HINT: In Haskell, you must surround string literals with double-quote '\"'. Such as \"Hello, world\"."

  it "given an answer with typo, show FAIL" $ do
    err <- ByteString.readFile "test/assets/4/error-messages/typo.txt"
    let e = setRunHaskellFailureWithOutput baseEnv err
    void (shouldFail =<< Exercise.verify subject e "test/assets/4/typo.hs")

  it "given an answer printing wrong result, show FAIL" $ do
    err <- ByteString.readFile "test/assets/4/error-messages/wrong-output.txt"
    let e = setRunHaskellSuccessWithStdout baseEnv err
    d <- shouldFail =<< Exercise.verify subject e "test/assets/4/wrong-output.hs"
    d `shouldSatisfy` Text.isInfixOf "Your program's output:" -- TODO: better output
    d `shouldSatisfy` Text.isInfixOf "Expected output:"
