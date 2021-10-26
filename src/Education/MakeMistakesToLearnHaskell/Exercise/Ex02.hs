{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex02
  ( exercise2
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types

exercise2 :: Exercise
exercise2 = Exercise "2" $ runHaskellExercise diag2 "20.761245674740486\n"

diag2 :: Diagnosis
diag2 code msg
  | "parse error" `Text.isInfixOf` msg || "Parse error" `Text.isInfixOf` msg =
    if "top-level declaration expected." `Text.isInfixOf` msg
      then
        "HINT: This error indicates that you haven't defined the main function."
      else
        -- TODO: Use regex or ghc tokenizer
        case compare (Text.count "(" code) (Text.count ")" code) of
            GT -> "HINT: you might have forgotten to write a close parenthesis"
            LT -> "HINT: you might have forgotten to write an open parenthesis"
            EQ -> ""
  | "No instance for (Fractional (IO ()))" `Text.isInfixOf` msg || "No instance for (Num (IO ()))" `Text.isInfixOf` msg =
    "HINT: you might have forgotten to write parentheses"
  | "No instance for (Show (a0 -> a0))" `Text.isInfixOf` msg =
    "HINT: you might have forgotten to write some numbers between operators ('*', '/' etc.)."
  | "No instance for (Num (t0 -> a0))" `Text.isInfixOf` msg =
    "HINT: you might have forgotten to write the multiplication operator '*'"
  | "No instance for (Fractional (t0 -> a0))" `Text.isInfixOf` msg =
    "HINT: you might have forgotten to write the division operator '/'"
  | "Variable not in scope: main :: IO" `Text.isInfixOf` msg =
    "HINT: This error indicates that you haven't defined the main function."
  | otherwise = ""
