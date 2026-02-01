{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.SpecHelper where

#include <test/imports/external.hs>

import qualified Education.MakeMistakesToLearnHaskell.Exercise as Exercise
import qualified Education.MakeMistakesToLearnHaskell.Exercise.FormatMessage as Exercise
import           Education.MakeMistakesToLearnHaskell.SpecEnv (setRunHaskellFailureWithOutput, mkDefaultSpecEnv)

shouldFail :: Exercise.Result -> IO Exercise.Details
shouldFail (Exercise.Fail _code d) = return $ Exercise.formatFailure d
shouldFail other = fail $ "Unexpected exercise result: " ++ show other


shouldSuccess :: Exercise.Result -> IO Exercise.Details
shouldSuccess (Exercise.Success d) = return d
shouldSuccess other = fail $ "Unexpected exercise result: " ++ show other


type TestCaseId = String

itShouldFailForCaseWithMessage :: Exercise.Name -> TestCaseId -> [Exercise.Details] -> SpecM () ()
itShouldFailForCaseWithMessage cname tcid messages = do
  baseEnv <- mkDefaultSpecEnv
  it (cname ++ "::" ++ tcid) $ do
    let subject = Exercise.unsafeGetByName cname
    err <- ByteString.readFile $ "test/assets/" ++ cname ++ "/error-messages/" ++ tcid ++ ".txt"
    let e = setRunHaskellFailureWithOutput baseEnv err
    d <- shouldFail =<< Exercise.verify subject e ("test/assets/" ++ cname ++ "/" ++ tcid ++ ".hs")

    for_ messages $ \message -> (Text.unpack d `shouldContain` Text.unpack message)