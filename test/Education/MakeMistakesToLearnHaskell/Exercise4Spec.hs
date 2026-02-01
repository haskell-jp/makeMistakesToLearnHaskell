{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise4Spec
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
  -- NOTE: SUCCESS and wrong-output case for exercise 4 is in the integration test
  -- to cover Education.MakeMistakesToLearnHaskell.Evaluator.RunHaskell.runFile with stdin.
  -- See Education.MakeMistakesToLearnHaskellSpec.

  itShouldFailForCaseWithMessage
    "4"
    "no-do"
    ["HINT: You seem to have forgotten to write `do`. `do` must be put before listing `putStr`s and `getContents`."]

  let useLeftThinArrow = "HINT: Don't assign the result of `getContents` with `=`. Use `<-` instead."
  -- ^ TODO: Collect common error messages

  itShouldFailForCaseWithMessage
    "4"
    "equal"
    [useLeftThinArrow]

  itShouldFailForCaseWithMessage
    "4"
    "equal-no-space"
    [useLeftThinArrow]

  itShouldFailForCaseWithMessage
    "4"
    "incornsistent-indent1"
    ["HINT: instructions in a `do` must be in a consistent width."]

  itShouldFailForCaseWithMessage
    "4"
    "incornsistent-indent2"
    ["HINT: instructions in a `do` must be in a consistent width."]

  itShouldFailForCaseWithMessage
    "4"
    "main-arrow"
    ["HINT: Don't use `<-` to define the `main` function. Use `=` instead."]

  itShouldFailForCaseWithMessage
    "4"
    "no-arrow"
    ["HINT: You have to assign the result of `getContents` with `<-` operator."]

  itShouldFailForCaseWithMessage
    "4"
    "no-close-paren"
    ["HINT: The open parenthesis between putStr and unlines is not closed."]

  itShouldFailForCaseWithMessage
    "4"
    "no-main"
    ["HINT: Your source code dosn't have `main` function!"]

  itShouldFailForCaseWithMessage
    "4"
    "no-open-paren1"
    ["HINT: No parentheses between putStr and unlines."]

  itShouldFailForCaseWithMessage
    "4"
    "no-open-paren2"
    ["HINT: No parentheses between unlines and reverse."]

  itShouldFailForCaseWithMessage
    "4"
    "no-open-paren3"
    ["HINT: No parentheses between reverse and lines."]

  itShouldFailForCaseWithMessage
    "4"
    "no-paren"
    [ "HINT: No parentheses between putStr and unlines"
    , "HINT: No parentheses between unlines and reverse."
    , "HINT: No parentheses between reverse and lines."
    ]

  itShouldFailForCaseWithMessage
    "4"
    "no-paren1"
    ["HINT: No parentheses between putStr and unlines."]

  itShouldFailForCaseWithMessage
    "4"
    "no-paren2"
    ["HINT: No parentheses between unlines and reverse."]

  itShouldFailForCaseWithMessage
    "4"
    "no-paren3"
    ["HINT: No parentheses between reverse and lines."]
