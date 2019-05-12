{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Evaluator.Command
  ( runFileWith
  , resolveHaskellProcessor
  , CommandError (..)
  ) where

#include <imports/external.hs>


import           Education.MakeMistakesToLearnHaskell.Env
import           Education.MakeMistakesToLearnHaskell.Evaluator.Types


runFileWith :: String -> [String] -> Env -> CommandParameters -> IO (Either CommandError (ByteString, ByteString))
runFileWith cname [] _e _rhp = return . Left $ CommandNotFound cname
runFileWith cname (actualCommand : initialArgs) e rhp = do
  let prc =
        Process.setStdin (Process.byteStringInput $ commandParametersStdin rhp)
          $ Process.proc actualCommand
          $ initialArgs ++ commandParametersArgs rhp
  (ecode, out, err) <- fixingCodePage e $ readProcess prc
  return $ case ecode of
      ExitSuccess -> Right (out, err)
      ExitFailure i -> Left $ CommandFailure cname i err


resolveHaskellProcessor :: String -> [String] -> IO [String]
resolveHaskellProcessor cname options = do
  stack <- Dir.findExecutable "stack"
  case stack of
      Just p -> return $ [p, "exec", cname, "--"] ++ options
      _ -> maybe [] (: options) <$> Dir.findExecutable cname


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
