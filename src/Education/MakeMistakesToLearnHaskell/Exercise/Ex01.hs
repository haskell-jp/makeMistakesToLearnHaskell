{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex01
  ( exercise1
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types

exercise1 :: Exercise
exercise1 =
  Exercise "1" $ runHaskellExerciseEq diag1 "Hello, world!\n"

diag1 :: Diagnosis
diag1 code msg
  | "parse error on input" `Text.isInfixOf` msg
      && "'" `Text.isInfixOf` code =
        "HINT: In Haskell, you must surround string literals with double-quotes '\"', like \"Hello, world\"."
  | ("parse error" `Text.isInfixOf` msg || "Parse error" `Text.isInfixOf` msg)
      && "top-level declaration expected." `Text.isInfixOf` msg =
        hintNoMain
  | "Variable not in scope: main :: IO" `Text.isInfixOf` msg =
    -- ^ Error message by runghc / runhaskell
    hintNoMain
  | "The IO action  emain f is not defined in module  eMain f" `Text.isInfixOf` msg =
    -- ^ Error message by ghc on Windows. Why are the quote characters converted into " e" and " f"?
    hintNoMain
  | "The IO action \8216main\8217 is not defined in module \8216Main\8217" `Text.isInfixOf` msg =
    -- ^ Error message by ghc on other OSs.
    hintNoMain
  | "Variable not in scope:" `Text.isInfixOf` msg =
    "HINT: you might have misspelled 'putStrLn'."
  | otherwise = ""
 where
  hintNoMain = "HINT: This error indicates that you haven't defined the main function."
