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
        let eResult = Ex21.formatResult
              <$> Error.note
                    "Fruit not found. He/She might like vegitables better."
                    (Map.lookup nameString Ex21.fruitDictionary)
              <*> Error.note
                    "Birthday not found. He/She might want to hide his/her age."
                    (Map.lookup nameString Ex21.birthdayDictionary)
              <*> Error.note
                    "Mail address not found. He/She might be afraid of spams."
                    (Map.lookup nameString Ex21.mailAddressDictionary)
              <*> Error.note
                    "Prefecture not found. Where does he/she live?"
                    (Map.lookup nameString Ex21.prefectureDictionary)
        case eResult of
            Right result -> Right result
            Left emsg    -> Right $ emsg <> "\n"
      _ -> Left $ "Invalid arguments: " <> Text.pack (show mereStrings)
 where
  mereStrings = map assertMereString args
