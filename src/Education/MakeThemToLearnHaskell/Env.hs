{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeThemToLearnHaskell.Env where

#include <imports/external.hs>

import           Education.MakeThemToLearnHaskell.Evaluator.Types

data Env =
  Env
    { logDebug :: ByteString -> IO ()
    , appHomePath :: FilePath
    , runHaskell :: Env -> FilePath -> IO (Either RunHaskellError (ByteString, ByteString))
    }


appName :: String
appName = "mmlh"


homePathEnvVarName :: String
homePathEnvVarName = "MAKE_THEM_TO_LEARN_HASKELL_HOME"


avoidCodingError :: IO ()
#ifdef mingw32_HOST_OS
avoidCodingError =
  IO.hSetEncoding IO.stdout $ mkLocaleEncoding TransliterateCodingFailure
#else
avoidCodingError = return ()
#endif
