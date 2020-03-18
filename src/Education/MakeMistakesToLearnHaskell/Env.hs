{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Env
  ( Env (..)
  , defaultEnv
  , CommandParameters (..)
  , appName
  , homePathEnvVarName
  , reportServerEnvVarName
  , avoidCodingError
  )
where

#include <imports/external.hs>
#include <imports/io.hs>

import           Education.MakeMistakesToLearnHaskell.Report.Client    (Report, Result(Result), ReportClientError)
import           Education.MakeMistakesToLearnHaskell.Evaluator.Types

data CommandParameters = CommandParameters
  { commandParametersArgs :: ![String]
  , commandParametersStdin :: !ByteString
  }

data Env = Env
  { logDebug :: ByteString -> IO ()
  , appHomePath :: FilePath
  , executeCommand :: String -> [FilePath] -> CommandParameters -> IO (Either CommandError ByteString)
  , confirm :: Text -> IO Bool
  , openWithBrowser :: Text -> IO Bool
  , say :: Text -> IO ()
  , envQcMaxSuccessSize :: Int
  , postReport :: Report -> IO (Either ReportClientError Result)
  }

defaultEnv :: Env
defaultEnv = Env
  { logDebug = error "Set logDebug to defaultEnv"
  , appHomePath = error "Set appHomePath to defaultEnv"
  , executeCommand = error "Set executeCommand to defaultEnv"
  , confirm =
      \prompt -> Text.putStrLn ("default Env.confirm: " <> prompt) >> return True
  , openWithBrowser =
      \url -> Text.putStrLn ("default Env.openWithBrowser: " <> url) >> return True
  , say = Text.putStrLn
  , envQcMaxSuccessSize = 20
  , postReport = \r -> do
      putStrLn $ "default postReport: " ++ show r
      return . Right $ Result "http://example.com/mmlh-reporter-example"
  }

appName :: String
appName = "mmlh"


homePathEnvVarName :: String
homePathEnvVarName = "MAKE_MISTAKES_TO_LEARN_HASKELL_HOME"


reportServerEnvVarName :: String
reportServerEnvVarName = "MAKE_MISTAKES_TO_LEARN_HASKELL_REPORT_SERVER"

avoidCodingError :: IO ()
#ifdef mingw32_HOST_OS
avoidCodingError =
  IO.hSetEncoding IO.stdout $ mkLocaleEncoding TransliterateCodingFailure
#else
avoidCodingError = return ()
#endif
