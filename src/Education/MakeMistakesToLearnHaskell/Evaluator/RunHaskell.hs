{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Evaluator.RunHaskell
  ( runFile
  ) where


#include <imports/external.hs>


import           Education.MakeMistakesToLearnHaskell.Env
import           Education.MakeMistakesToLearnHaskell.Evaluator.Types
import           Education.MakeMistakesToLearnHaskell.Evaluator.Command


runFile :: Env -> CommandParameters -> IO (Either CommandError (ByteString, ByteString))
runFile env params = do
  let ename = "runhaskell"
  commandAndArgs <- resolveHaskellProcessor ename optionsAlwaysColor
  runFileWith ename commandAndArgs env params


optionsAlwaysColor :: [String]
#ifdef mingw32_HOST_OS
-- FIXME: Command Prompt can't handle -fdiagnostics-color=always properly.
optionsAlwaysColor = []
#else
optionsAlwaysColor = ["--ghc-arg=-fdiagnostics-color=always"]
#endif
