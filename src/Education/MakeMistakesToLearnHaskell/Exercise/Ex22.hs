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
        let eResult = do
              personId <- Error.note
                "ID not found. Who is that person?"
                (Map.lookup (Text.pack nameString) Ex21.idDictionary)
              Ex21.formatResult
                <$> Error.note
                      "Fruit not found. He/She might like vegitables better."
                      (Map.lookup personId Ex21.fruitDictionary)
                <*> Error.note
                      "Birthday not found. He/She might want to hide his/her age."
                      (Map.lookup personId Ex21.birthdayDictionary)
        case eResult of
            Right result -> Right result
            Left emsg    -> Right $ emsg <> "\n"
      _ -> Left $ "Invalid arguments: " <> Text.pack (show mereStrings)
 where
  mereStrings = map assertMereString args
