{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Env
  ( Env (..)
  , VerifyCmdOutputLocation (..)
  , isBrowser
  , RunHaskellParameters(runHaskellParametersArgs, runHaskellParametersStdin)
  , defaultRunHaskellParameters
  , appName
  , homePathEnvVarName
  , avoidCodingError
  )
where

#include <imports/external.hs>

import           Education.MakeMistakesToLearnHaskell.Evaluator.Types

data RunHaskellParameters = RunHaskellParameters
  { runHaskellParametersArgs :: ![String]
  , runHaskellParametersStdin :: !ByteString
  }

defaultRunHaskellParameters :: RunHaskellParameters
defaultRunHaskellParameters = RunHaskellParameters [] ""

data Env =
  Env
    { logDebug :: ByteString -> IO ()
    , appHomePath :: FilePath
    , runHaskell :: RunHaskellParameters -> IO (Either RunHaskellError (ByteString, ByteString))
    , envQcMaxSuccessSize :: Int
    }

data VerifyCmdOutputLocation
  = Browser
  | Terminal
  deriving (Eq, Show, Read)

isBrowser :: VerifyCmdOutputLocation -> Bool
isBrowser Browser = True
isBrowser _ = False


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
