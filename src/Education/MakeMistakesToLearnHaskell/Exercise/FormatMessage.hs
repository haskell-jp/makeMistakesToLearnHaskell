{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.FormatMessage
  ( formatSingleArgFunApp
  , formatFailure
  ) where


#include <imports/external.hs>

import           Education.MakeMistakesToLearnHaskell.Exercise.Types
import           Education.MakeMistakesToLearnHaskell.Evaluator.Types


formatSingleArgFunApp :: SingleArgFunApp -> [Details]
formatSingleArgFunApp = List.unfoldr uf
  where
    uf safa1 =
      let f safa2 =
            let hint =
                  case singleArgFunAppHasParen safa2 of
                      BothParens    -> ""
                      NoParens      ->
                        "HINT: No parentheses between "
                          <> singleArgFunAppFunName safa1
                          <> " and "
                          <> singleArgFunAppFunName safa2
                          <> "."
                      OnlyOpenParen ->
                        "HINT: The open parenthesis between "
                          <> singleArgFunAppFunName safa1
                          <> " and "
                          <> singleArgFunAppFunName safa2
                          <> " is not closed."
            in
              -- NOTE: The final argument should not be tested because it should be a raw expression.
              --       E.g. The `input` argument of `reverse (lines input)`.
              if isJust $ singleArgFunAppArg safa2
                then Just (Text.fromStrict hint, safa2)
                else Nothing
      in f =<< singleArgFunAppArg safa1


formatFailure :: FailBy -> Details
formatFailure (WrongOutput details) = details
formatFailure (CommandFailed cname cout diag) =
  Text.intercalate "\n"
    [ "==================== " <> Text.pack cname <> " output ===================="
    , ""
    , cout
    , ""
    , "==================== mmlh HINT output ===================="
    , diag
    ]
