{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Report
  ( printUrlIfAsked
  ) where

#include <imports/external.hs>

import           Education.MakeMistakesToLearnHaskell.Report.Client    (Report (Report),
                                                                        reportUrl)

import           Education.MakeMistakesToLearnHaskell.Env
import qualified Education.MakeMistakesToLearnHaskell.Exercise.Types as Exercise


printUrlIfAsked :: Env -> Exercise.Name -> Exercise.SourceCode -> Exercise.FailBy -> IO ()
printUrlIfAsked e name code fb = do
  y <- confirm e "Report this failure to ask for a help?"
  when y $ do
    eurl <- fmap reportUrl <$> postReport e (Report name code fb)
    case eurl of
        Right url -> do
          _ <- openWithBrowser e url
          say e $ "Open " <> url <> " with your browser!"
        Left err -> do
          putStrLn "[WARN] Error when filing an issue at haskell-jp/makeMistakesToLearnHaskell-support."
          putStrLn "[WARN] This can be a bug. Please report at https://github.com/haskell-jp/makeMistakesToLearnHaskell/issues with the error message below:"
          print err
