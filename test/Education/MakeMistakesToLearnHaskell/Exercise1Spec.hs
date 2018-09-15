{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise1Spec
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
  itShouldFailForCaseWithMessage
    "1"
    "single-quote"
    ["HINT: In Haskell, you must surround string literals with double-quote '\"'. Such as \"Hello, world\"."]
  itShouldFailForCaseWithMessage "1" "typo" []
