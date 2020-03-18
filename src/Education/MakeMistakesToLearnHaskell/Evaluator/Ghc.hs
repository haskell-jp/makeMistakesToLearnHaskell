{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Evaluator.Ghc
  ( runHaskell
  ) where

#include <imports/external.hs>


import           Education.MakeMistakesToLearnHaskell.Env
import           Education.MakeMistakesToLearnHaskell.Evaluator.Types
import           Education.MakeMistakesToLearnHaskell.Evaluator.Command

runHaskell :: Env -> FilePath -> IO (Either GhcError CommandResult)
runHaskell env prgFile = do
  Temp.withSystemTempDirectory "mmlh-compiled-answer" $ \dir -> do
    let prg = FilePath.takeBaseName prgFile
        -- TODO: Hide `-o` option as implementation detail
        ghcParams = ["-o", dir </> prg, prgFile]
    commandAndArgs <- resolveHaskellProcessor cname (ghcParams ++ optionsAlwaysColor)
    case commandAndArgs of
        [] -> return $ Left GhcNotFound
        (actualCommand : args) -> do
          let params = CommandParameters args ""
          commandResult <- executeCommand env actualCommand params

runFile :: Env -> CommandParameters -> IO (Either CommandError ByteString)
runFile env params = do
  let cname = "ghc"
  commandAndArgs <- resolveHaskellProcessor cname ("-fno-code" : optionsAlwaysColor)
  runFileWith cname commandAndArgs env params


optionsAlwaysColor :: [String]
#ifdef mingw32_HOST_OS
-- FIXME: Command Prompt can't handle -fdiagnostics-color=always properly.
optionsAlwaysColor = []
#else
optionsAlwaysColor = ["-fdiagnostics-color=always"]
#endif
