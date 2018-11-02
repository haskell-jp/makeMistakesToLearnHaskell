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
    , runHaskell = error "Set runHaskell to defaultTestEnv!"
    , envQcMaxSuccessSize = 20
    }


setRunHaskellFailureWithOutput :: Env -> ByteString -> Env
setRunHaskellFailureWithOutput e err =
  e { runHaskell = \_path -> return $ Left $ RunHaskellFailure 1 err }


setRunHaskellSuccessWithStdout :: Env -> ByteString -> Env
setRunHaskellSuccessWithStdout e out =
  e { runHaskell = \_path -> return $ Right (out, "") }


setRunHaskellSuccessWithStdinFunction :: Env -> (ByteString -> ByteString) -> Env
setRunHaskellSuccessWithStdinFunction e func =
  e { runHaskell = \rhp -> return $ Right (func $ runHaskellParametersStdin rhp, "") }
