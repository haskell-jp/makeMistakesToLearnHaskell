{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.SpecEnv where


#include <test/imports/external.hs>

import           Education.MakeMistakesToLearnHaskell.Env
import           Education.MakeMistakesToLearnHaskell.Evaluator.Types


mkDefaultSpecEnv :: SpecM a Env
mkDefaultSpecEnv = runIO $ do
  tmpDir <- (</> "tmp/mmlh") <$> Dir.getCurrentDirectory
  return Env
    { logDebug = const $ return ()
    -- { logDebug = ByteString.putStrLn
    , appHomePath = tmpDir
    , runHaskell = error "Set runHaskell to defaultTestEnv!"
    }


setRunHaskellReturningOutput :: Env -> ByteString -> Env
setRunHaskellReturningOutput e err =
  e { runHaskell = \_env _path -> return $ Left $ RunHaskellFailure 1 err }
