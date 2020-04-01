{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Evaluator.Command
  ( execute
  ) where

#include <imports/external.hs>


import           Education.MakeMistakesToLearnHaskell.Env
import           Education.MakeMistakesToLearnHaskell.Evaluator.Types


execute :: CommandName -> CommandParameters -> IO CommandResult
execute cname cmdP = do
  let pathTpl = "mmlh-command-" ++ cname
  Temp.withSystemTempFile pathTpl $ \_path h -> do
    let prc =
          Process.setStdout (Process.useHandleOpen h)
            $ Process.setStderr (Process.useHandleOpen h)
            $ Process.setStdin (Process.byteStringInput $ commandParametersStdin cmdP)
            $ Process.proc cname
            $ commandParametersArgs cmdP
    ecode <- fixingCodePage $ runProcess prc
    IO.hSeek h IO.AbsoluteSeek 0
    CommandResult ecode <$> ByteString.hGetContents h


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
