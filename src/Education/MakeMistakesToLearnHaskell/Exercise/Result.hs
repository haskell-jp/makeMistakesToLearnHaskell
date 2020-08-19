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
                , headerYourProgramsOutput <> Text.pack (show out) -- TODO: pretty print
                , headerExpectedOutput <> Text.pack (show right)
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


headerYourProgramsOutput, headerExpectedOutput, headerForInput, headerForArgs, headerForShowArg, headerForShowLine :: Text
headerYourProgramsOutput = "Your program's output: "
headerExpectedOutput     = "      Expected output: "
headerForInput           = "            For input: "
headerForArgs            = "            For  args: "
headerForShowArg         = "             "
headerForShowLine        = "              "


whenGhcNotFound :: Result
whenGhcNotFound =
  Error $ Text.pack "ghc command is not available.\nInstall stack or Haskell Platform."


whenGhcError :: SourceCode -> Details -> Details -> Result
whenGhcError code ghcMsg diagnosisMsg =
  Fail code $ CompileError ghcMsg diagnosisMsg


messageFooter :: Text -> [CommandLineArg] -> [Text]
messageFooter "" [] = []
messageFooter input args =
  [headerForInput <> formattedInput, headerForArgs <> formattedArgs]
 where
  formattedInput =
    if Text.null input then "<no input>" else Text.pack (show input)

  formattedArgs
    | null args = "<no arguments>"
    | any isFilePath args = "\n" <> multiline
    | otherwise = Text.pack (show $ map assertMereString args)
  multiline = Text.concat $ zipWith showArg [1..] args

  showArg :: Int -> CommandLineArg -> Text
  showArg i (Mere s) = Text.pack (show i) <> ": " <> Text.pack (show s) <> "\n"
  showArg i (FilePath path content) =
    headerForShowArg <> Text.pack (show i) <> ": file " <> Text.pack (show path) <> "\n" <> showContent content

  showContent content = Text.unlines . map showLine $ Text.lines content
  showLine line = headerForShowLine <> Text.pack (show line)


resultForNotYetImplementedVeirificationExercise :: SourceCode -> Either GhcError () -> IO (Result)
resultForNotYetImplementedVeirificationExercise code result =
  case result of
      Right _ ->
        return NotYetImplemented
      Left (GhcError _ecode msg) ->
        return $ whenGhcError code (decodeUtf8 msg) ""
      Left GhcNotFound ->
        return whenGhcNotFound
