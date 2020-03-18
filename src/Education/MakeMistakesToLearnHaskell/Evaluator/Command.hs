{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Evaluator.Command
  ( execute
  , resolveHaskellProcessor
  , CommandError (..)
  ) where

#include <imports/external.hs>


import           Education.MakeMistakesToLearnHaskell.Env
import           Education.MakeMistakesToLearnHaskell.Evaluator.Types


-- TODO: Make a variant of execute returning both stderr and stdout
--       Make runGhc return message by compiler and the compiled command
--       Drop runHaskell?
-- TODO: member of Env
-- TODO: Don't handle CommandError as Fail: Judge should receive Exitcode
execute :: CommandName -> [String] -> CommandParameters -> IO (Either CommandError ByteString)
execute cname [] _rhp = return . Left $ CommandNotFound cname
execute cname (actualCommand : initialArgs) cmdP = do
  let pathTpl = "mmlh-command-" ++ cname
  Temp.withSystemTempFile pathTpl $ \_path h -> do
    let prc =
          Process.setStdout (Process.useHandleOpen h)
            $ Process.setStderr (Process.useHandleOpen h)
            $ Process.setStdin (Process.byteStringInput $ commandParametersStdin cmdP)
            $ Process.proc actualCommand
            $ initialArgs ++ commandParametersArgs cmdP
    ecode <- fixingCodePage $ runProcess prc
    IO.hSeek h IO.AbsoluteSeek 0
    out <- ByteString.hGetContents h
    return $ case ecode of
        ExitSuccess -> Right out
        ExitFailure i -> Left $ CommandFailure cname i out


resolveHaskellProcessor :: CommandName -> [String] -> IO [String]
resolveHaskellProcessor cname options = do
  stack <- Dir.findExecutable "stack"
  case stack of
      Just p -> return $ [p, "exec", cname, "--"] ++ options
      _ -> maybe [] (: options) <$> Dir.findExecutable cname


-- | Ref: https://github.com/commercialhaskell/stack/blob/a9042ad6fa1d7c813a1c79713a518ee521da9add/src/Stack/Build.hs#L306-L332
fixingCodePage :: IO a -> IO a
#ifdef mingw32_HOST_OS
fixingCodePage action = do
  cpInSave <- Win32.getConsoleCP
  cpOutSave <- Win32.getConsoleOutputCP
  let utf8 = 65001

      fixingInput act =
        if cpInSave /= utf8
          then
            bracket_
              (Win32.setConsoleCP utf8)
              (Win32.setConsoleCP cpInSave)
              act
          else
            act

      fixingOutput act =
        if cpOutSave /= utf8
          then
            bracket_
              (Win32.setConsoleOutputCP utf8)
              (Win32.setConsoleOutputCP cpOutSave)
              act
          else
            act

  fixingInput $ fixingOutput action
#else
fixingCodePage _ action = action
#endif
