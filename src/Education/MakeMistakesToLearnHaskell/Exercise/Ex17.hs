{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex17
  ( exercise17
  , stdinGenerator
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types


exercise17 :: Exercise
exercise17 = Exercise "17"
           $ runHaskellExerciseWithStdinEq diag answer stdinGenerator


diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented


stdinGenerator :: Gen Text
stdinGenerator = do
  let addCommand = QuickCheck.oneof [validAddCommand, invalidAddCommand]
      getCommand = QuickCheck.oneof [validGetCommand, invalidGetCommand]
  inputLines <- QuickCheck.listOf $ QuickCheck.oneof [addCommand, getCommand, randomLine]
  return . Text.unlines $ inputLines ++ ["quit"]

 where
  validAddCommand = do
    k <- key
    val <- arbitrary :: Gen Integer
    return $ Text.unwords ["add", k, Text.pack $ show val]

  invalidAddCommand =
    Text.unwords <$> QuickCheck.oneof [invalidAdd0, invalidAdd1, invalidAdd2, invalidAdd3OrMore]

  invalidAdd0 = pure ["add"]

  invalidAdd1 = do
    k <- randomWord
    return ["add", k]

  invalidAdd2 = do
    k <- randomWord
    v <- randomWord
    return ["add", k, v]

  invalidAdd3OrMore = do
    k <- randomWord
    v <- arbitrary :: Gen Integer
    leftWords <- randomWords
    return $ ["add", k, Text.pack $ show v] ++ leftWords

  validGetCommand = do
    k <- key
    return $ Text.unwords ["get", k]

  invalidGetCommand =
    Text.unwords <$> QuickCheck.oneof [invalidGet0, invalidGet2OrMore]

  invalidGet0 = pure ["get"]

  invalidGet2OrMore = do
    k <- randomWord
    leftWords <- randomWords1
    return $ ["get", k] ++ leftWords

  key = Text.singleton <$> QuickCheck.elements ['A' .. 'E']

  randomLine = do
    xWord <- ("X" <>) <$> randomWord
    leftWords <- randomWords
    return . Text.unwords $ [xWord] ++ leftWords

  randomWord =
    fmap Text.pack
      . QuickCheck.listOf1
      . QuickCheck.elements $ ['A'..'Z'] ++ ['a'..'z']

  randomWords = QuickCheck.listOf randomWord
  randomWords1 = QuickCheck.listOf1 randomWord


answer :: Text -> Text
answer = Text.unlines . go Map.empty . Text.lines
 where
  go _ [] = error $ "Assertion failure: empty input given before \"quit\" command."
  go db (first : left) =
    let prompt = "Enter command:"
        mcommand = parseCommand first
        (result, newDb) =
          case mcommand of
              Just (Add k v) ->
                let message = "Adding " <> Text.pack (show v) <> " to " <> Text.pack (show k) <> "."
                 in (prompt : [message], Map.insertWith (\v1 v2 -> v1 + v2) k v db)
              Just (Get k) ->
                let message =
                      case Map.lookup k db of
                          Just v -> k <> " => " <> Text.pack (show v)
                          Nothing -> "Error: no item found"
                 in (prompt : [message], db)
              Just Quit ->
                (prompt : ["Bye."], db)
              Nothing ->
                (prompt : ["Error: Invalid Command"], db)
     in
      result ++ go newDb left

data Command =
    Add Text Integer
  | Get Text
  | Quit

parseCommand :: Text -> Maybe Command
parseCommand s =
  case Text.words s of
      ["add", k, mv] ->
        case Error.readMay $ Text.unpack mv of
            Just v -> Just (Add k v)
            Nothing -> Nothing
      ["get", k] -> Just (Get k)
      ["quit"] -> Just Quit
      _ -> Nothing
