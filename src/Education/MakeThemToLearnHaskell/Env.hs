{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeThemToLearnHaskell.Env
  ( Env(..)
  , withEnv
  ) where

#include <imports/external.hs>


data Env =
  Env
    { logDebug :: ByteString -> IO ()
    , appHomePath :: FilePath
    }


withEnv :: (Env -> IO r) -> IO r
withEnv k = do
  d <- Env.getEnv homePathEnvVarName <|> Dir.getXdgDirectory Dir.XdgData appName
  Dir.createDirectoryIfMissing True d
  IO.withFile (d </> "debug.log") IO.WriteMode $ \h ->
    k $ Env (ByteString.hPutStr h . (<> "\n")) d


homePathEnvVarName :: String
homePathEnvVarName = "MAKE_THEM_TO_LEARN_HASKELL_HOME"


appName :: String
appName = "mmlh"
