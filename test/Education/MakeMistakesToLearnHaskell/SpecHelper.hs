{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.SpecHelper where

#include <test/imports/external.hs>

import qualified Education.MakeMistakesToLearnHaskell.Exercise as Exercise


shouldFail :: Exercise.Result -> IO Exercise.Details
shouldFail (Exercise.Fail d) = return d
shouldFail (Exercise.Success d) = fail $ "Unexpected Success: " ++ show d
shouldFail (Exercise.Error d) = fail $ "Unexpected Error: " ++ show d


shouldSuccess :: Exercise.Result -> IO Exercise.Details
shouldSuccess (Exercise.Fail d) = fail $ "Unexpected Fail: " ++ show d
shouldSuccess (Exercise.Success d) = return d
shouldSuccess (Exercise.Error d) = fail $ "Unexpected Error: " ++ show d
