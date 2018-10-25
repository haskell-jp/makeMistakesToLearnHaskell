{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.SpecHelper where

#include <test/imports/external.hs>

import qualified Education.MakeMistakesToLearnHaskell.Exercise as Exercise
import           Education.MakeMistakesToLearnHaskell.SpecEnv (setRunHaskellFailureWithOutput, mkDefaultSpecEnv)


shouldFail :: Exercise.Result -> IO Exercise.Details
shouldFail (Exercise.Fail d) = return d
shouldFail other = fail $ "Unexpected exercise result: " ++ show other


shouldSuccess :: Exercise.Result -> IO Exercise.Details
shouldSuccess (Exercise.Success d) = return d
shouldSuccess other = fail $ "Unexpected exercise result: " ++ show other


type TestCaseId = String

itShouldFailForCaseWithMessage :: Exercise.Name -> TestCaseId -> [Exercise.Details] -> SpecM () ()
itShouldFailForCaseWithMessage ename tcid messages = do
  baseEnv <- mkDefaultSpecEnv
  it (ename ++ "::" ++ tcid) $ do
    let subject = Exercise.unsafeGetByName ename
    err <- ByteString.readFile $ "test/assets/" ++ ename ++ "/error-messages/" ++ tcid ++ ".txt"
    let e = setRunHaskellFailureWithOutput baseEnv err
    d <- shouldFail =<< Exercise.verify subject e ("test/assets/" ++ ename ++ "/" ++ tcid ++ ".hs")

    for_ messages $ \message -> d `shouldSatisfy` Text.isInfixOf message
