{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Evaluator.Command
  ( execute
  ) where

#include <imports/external.hs>

import           Education.MakeMistakesToLearnHaskell.Env
import           Education.MakeMistakesToLearnHaskell.Evaluator.Types


execute :: CommandName -> CommandParameters -> IO CommandResult
execute cname cmdP = do
  let prc =
        Process.setStdin (Process.byteStringInput . ByteStringLazy.fromStrict $ commandParametersStdin cmdP)
          $ Process.proc cname
          $ commandParametersArgs cmdP
  (ecode, out) <- fixingCodePage $ readProcessInterleaved prc
  return . CommandResult ecode $ ByteStringLazy.toStrict out


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
fixingCodePage action = action
#endif
