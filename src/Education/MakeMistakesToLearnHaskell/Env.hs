{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Env
  ( Env (..)
  , defaultEnv
  , CommandParameters(commandParametersArgs, commandParametersStdin)
  , defaultRunHaskellParameters
  , appName
  , homePathEnvVarName
  , avoidCodingError
  )
where

#include <imports/external.hs>

import           Education.MakeMistakesToLearnHaskell.Evaluator.Types

data CommandParameters = CommandParameters
  { commandParametersArgs :: ![String]
  , commandParametersStdin :: !ByteString
  }

defaultRunHaskellParameters :: CommandParameters
defaultRunHaskellParameters = CommandParameters [] ""

data Env = Env
  { logDebug :: ByteString -> IO ()
  , appHomePath :: FilePath
  , runHaskell :: CommandParameters -> IO (Either CommandError (ByteString, ByteString))
  , runGhc :: CommandParameters -> IO (Either CommandError (ByteString, ByteString))
  , envQcMaxSuccessSize :: Int
  }

defaultEnv :: Env
defaultEnv = Env
  { logDebug = error "Set logDebug to defaultEnv"
  , appHomePath = error "Set appHomePath to defaultEnv"
  , runHaskell = error "Set runHaskell to defaultEnv"
  , runGhc = error "Set runGhc to defaultEnv"
  , envQcMaxSuccessSize = 20
  }

appName :: String
appName = "mmlh"


homePathEnvVarName :: String
homePathEnvVarName = "MAKE_MISTAKES_TO_LEARN_HASKELL_HOME"

avoidCodingError :: IO ()
#ifdef mingw32_HOST_OS
avoidCodingError =
  IO.hSetEncoding IO.stdout $ mkLocaleEncoding TransliterateCodingFailure
#else
avoidCodingError = return ()
#endif
