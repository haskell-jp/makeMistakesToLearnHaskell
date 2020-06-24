{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.SpecEnv where


#include <test/imports/external.hs>

import           Education.MakeMistakesToLearnHaskell.Env
import           Education.MakeMistakesToLearnHaskell.Evaluator.Types


mkDefaultSpecEnv :: SpecM a Env
mkDefaultSpecEnv = runIO $ do
  tmpDir <- (</> "tmp/mmlh") <$> Dir.getCurrentDirectory
  return $ defaultEnv
            { logDebug = const $ return ()
            -- { logDebug = ByteString.putStrLn
            , appHomePath = tmpDir
            }


setRunHaskellFailureWithOutput :: Env -> ByteString'.ByteString -> Env
setRunHaskellFailureWithOutput e err =
  e { executeCommand = \_cname _params -> return $ CommandResult (ExitFailure 1) err }


setRunHaskellSuccessWithStdout :: Env -> ByteString'.ByteString -> Env
setRunHaskellSuccessWithStdout e out =
  e { executeCommand = \_cname _params -> return $ CommandResult ExitSuccess out }


setRunHaskellSuccessWithStdinFunction :: Env -> (ByteString'.ByteString -> ByteString'.ByteString) -> Env
setRunHaskellSuccessWithStdinFunction e func =
  e {
    executeCommand = \_cname params ->
      return . CommandResult ExitSuccess . func $ commandParametersStdin params
  }
