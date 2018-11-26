{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex04
  ( exercise4
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Evaluator.Regex
import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.FormatMessage
import Education.MakeMistakesToLearnHaskell.Exercise.Types

exercise4 :: Exercise
exercise4 = Exercise "4"
          $ runHaskellExerciseWithStdin diag4 gen4
          $ (Text.pack . unlines . reverse . lines . Text.unpack)

gen4 :: Gen String
gen4 = unlines <$> QuickCheck.listOf (QuickCheck.listOf $ QuickCheck.choose ('\33', '\126'))

diag4 :: Diagnosis
diag4 code msg
  | code `isInconsistentlyIndentedAfter` "do" =
    detailsDoConsistentWidth
  | "Perhaps this statement should be within a 'do' block?" `Text.isInfixOf` msg =
    if hasNoMainFirst code then
      "HINT: Your source code dosn't have `main` function!" -- TODO: Rewrite other no-main cases with this.
    else if code `containsSequence` ["main", "<-"] then
      "HINT: Don't use `<-` to define the `main` function. Use `=` instead."
    else
      detailsForgetToWriteDo "`putStr`s and `getContents`"
  | "Perhaps you need a 'let' in a 'do' block?" `Text.isInfixOf` msg
    && code `containsSequence` ["=", "getContents"] =
      "HINT: Don't assign the result of `getContents` with `=`. Use `<-` instead."
  | "Couldn't match type ‘IO String’ with ‘[Char]’" `Text.isInfixOf` msg
    && "In the first argument of ‘lines’" `Text.isInfixOf` msg =
      "HINT: Unfortunately, you have to assign the result of `getContents` with `<-` operator."
  | otherwise =
    let mtoks = GHC.tokenizeHaskell (Text.toStrict code)
        tokPutStr = (GHC.VariableTok, "putStr")
        putStrThenSpace =
          Regex.sym tokPutStr <* optional (Regex.psym ((== GHC.SpaceTok) . fst))
        msafa =
          matchSub (singleArgFunApp 5) . (tokPutStr :) . dropUntilFirst putStrThenSpace =<< mtoks
    in
      case msafa of
          Just safa -> Text.unlines $ formatSingleArgFunApp safa
          _ -> ""

-- TODO: Incomplete implementaion! Use regex or ghc tokenizer!
containsSequence :: SourceCode -> [Text] -> Bool
containsSequence code wds = Text.concat wds `isInWords` ws || (wds `List.isInfixOf` ws)
  where
    ws = Text.words code

hasNoMainFirst :: SourceCode -> Bool
hasNoMainFirst src =
  case Text.words src of
      [] -> True
      (h : _) -> not $ "main" `Text.isPrefixOf` h
