{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Evaluator.RunHaskell
  ( runFile
  , RunHaskellError(..)
  ) where


#include <imports/external.hs>


import           Education.MakeMistakesToLearnHaskell.Env
import           Education.MakeMistakesToLearnHaskell.Evaluator.Types


runFile :: Env -> RunHaskellParameters -> IO (Either RunHaskellError (ByteString, ByteString))
runFile e rhp = do
  cmd <- resolveInterpreter
  case cmd of
      [] -> return $ Left RunHaskellNotFound
      (h:left) -> do
        let prc =
              Process.setStdin (Process.byteStringInput $ runHaskellParametersStdin rhp)
                $ Process.proc h
                $ left ++ runHaskellParametersArgs rhp
        (ecode, out, err) <- fixingCodePage e $ readProcess prc
        return $ case ecode of
            ExitSuccess -> Right (out, err)
            ExitFailure i -> Left $ RunHaskellFailure i err


resolveInterpreter :: IO [String]
resolveInterpreter = do
  stack <- Dir.findExecutable "stack"
  case stack of
      Just p -> return $ [p, "exec", executableName, "--"] ++ optionsAlwaysColor
      _ -> maybe [] (: optionsAlwaysColor) <$> Dir.findExecutable executableName


optionsAlwaysColor :: [String]
#ifdef mingw32_HOST_OS
-- FIXME: Command Prompt can't handle -fdiagnostics-color=always properly.
optionsAlwaysColor = []
#else
optionsAlwaysColor = ["--ghc-arg=-fdiagnostics-color=always"]
#endif


executableName :: String
executableName = "runhaskell"

-- | Ref: https://github.com/commercialhaskell/stack/blob/a9042ad6fa1d7c813a1c79713a518ee521da9add/src/Stack/Build.hs#L306-L332
fixingCodePage :: Env -> IO a -> IO a
#ifdef mingw32_HOST_OS
fixingCodePage e action = do
  cpInSave <- Win32.getConsoleCP
  cpOutSave <- Win32.getConsoleOutputCP
  -- TODO: delete to independent from Env
  logDebug e $ "Fixing ConsoleCP from " <> ByteString.pack (show cpInSave)
  logDebug e $ "Fixing ConsoleOutputCP from " <> ByteString.pack (show cpOutSave)

  let utf8 = 65001
      setInput = cpInSave /= utf8
      setOutput = cpOutSave /= utf8
      fixingInput
        | setInput = bracket_
            (Win32.setConsoleCP utf8)
            (Win32.setConsoleCP cpInSave)
        | otherwise = id
      fixingOutput
        | setOutput = bracket_
              (Win32.setConsoleOutputCP utf8)
              (Win32.setConsoleOutputCP cpOutSave)
        | otherwise = id
  fixingInput $ fixingOutput action
#else
fixingCodePage _ = id
#endif
