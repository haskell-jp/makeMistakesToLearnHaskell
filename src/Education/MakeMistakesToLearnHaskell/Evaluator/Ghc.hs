{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Evaluator.Ghc
  ( runHaskell
  , compileWithGhc
  , checkWithGhc
  ) where

#include <imports/external.hs>


import           Education.MakeMistakesToLearnHaskell.Env
import           Education.MakeMistakesToLearnHaskell.Evaluator.Types
import           Education.MakeMistakesToLearnHaskell.Evaluator.Command

runHaskell :: Env -> FilePath -> IO (Either GhcError CommandResult)
runHaskell env srcPath =
  Temp.withSystemTempDirectory "mmlh-compiled-answer" $ \dir -> do
    ecompiledPath <- compileWithGhc env dir srcPath
    for ecompiledPath $ \compiledPath -> do
        let params = CommandParameters [] ""
        executeCommand env compiledPath params


compileWithGhc :: Env -> FilePath -> FilePath -> IO (Either GhcError FilePath)
compileWithGhc env outDir srcPath = do
  let prg = FilePath.takeBaseName srcPath
      compiledPath = outDir </> prg
      ghcArgs = ["-o", compiledPath, srcPath]
  ghcCommandAndArgs <- resolveGhc $ ghcArgs ++ optionsAlwaysColor
  case ghcCommandAndArgs of
      [] -> return $ Left GhcNotFound
      (actualCommand : args) -> do
        let ghcParams = CommandParameters args ""
        ghcResult <- executeCommand env actualCommand ghcParams
        case ghcResult of
            CommandResult ExitSuccess _out ->
              return $ Right compiledPath
            CommandResult (ExitFailure ecode) out ->
              return . Left $ GhcError ecode out


checkWithGhc :: Env -> FilePath -> IO (Either GhcError ())
checkWithGhc env srcPath = do
  let ghcArgs = ["-fno-code", srcPath]
  ghcCommandAndArgs <- resolveGhc $ ghcArgs ++ optionsAlwaysColor
  case ghcCommandAndArgs of
      [] -> return $ Left GhcNotFound
      (actualCommand : args) -> do
        let ghcParams = CommandParameters args ""
        ghcResult <- executeCommand env actualCommand ghcParams
        case ghcResult of
            CommandResult ExitSuccess _out ->
              return $ Right ()
            CommandResult (ExitFailure ecode) out ->
              return . Left $ GhcError ecode out


resolveGhc :: [String] -> IO [String]
resolveGhc options = do
  let cname = "ghc"
  stack <- Dir.findExecutable "stack"
  case stack of
      Just p -> return $ [p, "exec", cname, "--"] ++ options
      _ -> maybe [] (: options) <$> Dir.findExecutable cname


optionsAlwaysColor :: [String]
#ifdef mingw32_HOST_OS
-- FIXME: Command Prompt can't handle -fdiagnostics-color=always properly.
optionsAlwaysColor = []
#else
optionsAlwaysColor = ["-fdiagnostics-color=always"]
#endif
