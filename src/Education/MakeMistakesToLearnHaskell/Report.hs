{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Report
  ( buildUrl
  , buildBody
  , printUrlIfAsked
  ) where

#include <imports/external.hs>

import           Education.MakeMistakesToLearnHaskell.Env
import qualified Education.MakeMistakesToLearnHaskell.Exercise.Types as Exercise


buildUrl :: Exercise.Name -> Exercise.SourceCode -> Exercise.FailBy -> Text
buildUrl name code fb =
  "https://github.com/haskell-jp/makeMistakesToLearnHaskell-support/issues/new?labels=support&title=Exercise%20"
    <> Text.fromStrict (Uri.encodeText $ TextS.pack name)
    <> "&body="
    <> Text.fromStrict (Uri.encodeText . Text.toStrict $ buildBody code fb)


buildBody :: Exercise.SourceCode -> Exercise.FailBy -> Text
buildBody code fb = Text.unlines
  ["# Answer"
  , ""
  , "```"
  , code
  , "```"
  , ""
  , case fb of
        Exercise.WrongOutput out -> Text.unlines
          [ "# Cause: Wrong Output"
          , ""
          , "```"
          , out
          , "```"
          ]
        Exercise.CommandFailed cname cout diag -> Text.unlines
          [ "# Cause: `" <> Text.pack cname <> "` failed."
          , ""
          , "```"
          , cout
          , "```"
          , ""
          , if Text.null diag
              then "## No Diagnosis"
              else Text.unlines
                [ "## Diagnosis"
                , ""
                , "```"
                , diag
                , "```"
                ]
          ]
  ]


printUrlIfAsked :: Env -> Exercise.Name -> Exercise.SourceCode -> Exercise.FailBy -> IO ()
printUrlIfAsked e name code fb = do
  y <- confirm e "Report this failure to ask for a help?"
  when y $ do
    let url = buildUrl name code fb
    _ <- openWithBrowser e url
    say e $ "Open " <> url <> " with your browser!"
