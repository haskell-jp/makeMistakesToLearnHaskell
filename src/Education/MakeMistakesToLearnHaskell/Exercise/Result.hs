{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Result
  ( resultForUser
  , resultForNotYetImplementedVeirificationExercise
  , whenGhcNotFound
  , whenGhcError
  ) where

#include <imports/external.hs>

import           Education.MakeMistakesToLearnHaskell.Exercise.CommandLineArg
import           Education.MakeMistakesToLearnHaskell.Exercise.Types
import           Education.MakeMistakesToLearnHaskell.Evaluator.Types
import           Education.MakeMistakesToLearnHaskell.Text


resultForUser
  :: Diagnosis
  -> Text
  -> Judge
  -> [CommandLineArg]
  -> Text
  -> Either GhcError CommandResult
  -> Result
resultForUser diag code judge args input result =
  case result of
      Right (CommandResult ecode outB) ->
        let out = canonicalizeNewlines outB
            (right, isSuccessful) = judge args input ecode out
            msg =
              Text.unlines $
                [ Text.replicate 80 "="
                , "Your program's output: " <> Text.pack (show out) -- TODO: pretty print
                , "      Expected output: " <> Text.pack (show right)
                ] ++ messageFooter input args
        in
          if isSuccessful
            then Success $ "Nice output!\n\n" <> msg
            else Fail code . WrongOutput $ "Wrong output!\n\n" <> msg
      Left GhcNotFound -> -- TODO: duplicate error handling code with Exercise.Core
        whenGhcNotFound
      Left (GhcError _ msg) ->
        let textMsg = decodeUtf8 msg
         in whenGhcError code textMsg $ diag code textMsg


whenGhcNotFound :: Result
whenGhcNotFound =
  Error $ Text.pack "ghc command is not available.\nInstall stack or Haskell Platform."


whenGhcError :: SourceCode -> Details -> Details -> Result
whenGhcError code ghcMsg diagnosisMsg =
  Fail code $ CompileError ghcMsg diagnosisMsg


messageFooter :: Text -> [CommandLineArg] -> [Text]
messageFooter "" [] = []
messageFooter input args =
  [ "            For input: " <> Text.pack (show input)
  , "            For args:" <> formattedArgs
  ]
 where
  formattedArgs =
    if any isFilePath args
      then "\n" <> multiline
      else " " <> Text.pack (show $ map assertMereString args)
  multiline =

  -- TODO: ^ Show argumens in a more readable way:
    -- Separate lines when args contain at least one FilePath


resultForNotYetImplementedVeirificationExercise :: SourceCode -> Either GhcError () -> IO (Result)
resultForNotYetImplementedVeirificationExercise code result =
  case result of
      Right _ ->
        return NotYetImplemented
      Left (GhcError _ecode msg) ->
        return $ whenGhcError code (decodeUtf8 msg) ""
      Left GhcNotFound ->
        return whenGhcNotFound
