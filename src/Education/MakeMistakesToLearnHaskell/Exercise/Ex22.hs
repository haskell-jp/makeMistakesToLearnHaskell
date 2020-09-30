{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex22
  ( exercise22
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.CommandLineArg
import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types

import qualified Education.MakeMistakesToLearnHaskell.Exercise.Ex21 as Ex21


exercise22 :: Exercise
exercise22 = Exercise "22"
           $ runHaskellExerciseWithArgsAndStdin diag (judgeWithException answer) Ex21.argsGenerator Ex21.stdinGenerator


diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented


answer :: [CommandLineArg] -> Text -> Either Text Text
answer args _input =
  case mereStrings of
      [nameString] -> do
        let mResult = Ex21.formatResult
              <$> error "Use the note function of the error package" (Map.lookup nameString Ex21.fruitDictionary)
              <*> error "Use the note function of the error package" (Map.lookup nameString Ex21.birthdayDictionary)
              <*> error "Use the note function of the error package" (Map.lookup nameString Ex21.mailAddressDictionary)
              <*> error "Use the note function of the error package" (Map.lookup nameString Ex21.prefectureDictionary)
        case mResult of
            Just result -> Right result
            Nothing     -> Right "Not found. He/She might be shy.\n"
      _ -> Left $ "Invalid arguments: " <> Text.pack (show mereStrings)
 where
  mereStrings = map assertMereString args
