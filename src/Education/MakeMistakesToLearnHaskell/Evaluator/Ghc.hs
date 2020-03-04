{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Evaluator.Ghc
  ( runFile
  ) where

#include <imports/external.hs>


import           Education.MakeMistakesToLearnHaskell.Env
import           Education.MakeMistakesToLearnHaskell.Evaluator.Types
import           Education.MakeMistakesToLearnHaskell.Evaluator.Command


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
