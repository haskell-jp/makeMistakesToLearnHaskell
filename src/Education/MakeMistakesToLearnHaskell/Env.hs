{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Env
  ( Env (..)
  , defaultEnv
  , VerifyCmdOutputLocation (..)
  , RunHaskellParameters(runHaskellParametersArgs, runHaskellParametersStdin)
  , defaultRunHaskellParameters
  , appName
  , homePathEnvVarName
  , terminalOutputEnvVarName
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

data Env = Env
  { logDebug :: ByteString -> IO ()
  , appHomePath :: FilePath
  , runHaskell :: RunHaskellParameters -> IO (Either RunHaskellError (ByteString, ByteString))
  , envQcMaxSuccessSize :: Int
  , envVerifyOutputLocation :: VerifyCmdOutputLocation -- ^ verify コマンドの出力先
  }

data VerifyCmdOutputLocation
  = Browser
  | Terminal
  deriving (Eq, Show, Read)

defaultEnv :: Env
defaultEnv = Env
  { logDebug = error "Set logDebug to defaultEnv"
  , appHomePath = error "Set appHomePath to defaultEnv"
  , runHaskell = error "Set runHaskell to defaultEnv"
  , envQcMaxSuccessSize = 20
  , envVerifyOutputLocation = Browser
  }

appName :: String
appName = "mmlh"


homePathEnvVarName :: String
homePathEnvVarName = "MAKE_MISTAKES_TO_LEARN_HASKELL_HOME"

terminalOutputEnvVarName :: String
terminalOutputEnvVarName = "MAKE_MISTAKES_TO_LEARN_HASKELL_TERMINAL_OUTPUT_LOCATION"


avoidCodingError :: IO ()
#ifdef mingw32_HOST_OS
avoidCodingError =
  IO.hSetEncoding IO.stdout $ mkLocaleEncoding TransliterateCodingFailure
#else
avoidCodingError = return ()
#endif
