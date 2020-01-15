{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex09
  ( exercise9
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types


exercise9 :: Exercise
exercise9 = Exercise "9"
          $ runHaskellExerciseWithStdin diag generator answer


diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented


-- TODO: 2個の数値を生成して、区切り文字を変える
--       異常系: 数値が一つも生成されない
--               数値が一つだけ生成される => read関数が例外を投げる。今回は例外を想定しないので、想定しないケース
generator :: Gen String
generator = do
  input1 <- QuickCheck.listOf $ show . QuickCheck.getPositive <$> (arbitrary :: Gen (QuickCheck.Positive Double))
  case input1 of
      [height] -> do
          input2 <- show . QuickCheck.getPositive <$> (arbitrary :: Gen (QuickCheck.Positive Double))
          return $ unlines [height, input2]
      _ -> return $ unwords input1 <> "\n"


answer :: Text -> Text
answer input = Text.pack $ "Height Weight: \n" <> a <>"\n"
 where
  a = case lines $ Text.unpack input of
    [""] -> "Invalid input: "
    [line1] -> show ( weight / (height * height))
      where heightStr : weightStr : _ = words line1
            height, weight :: Double
            height = read heightStr
            weight = read weightStr
    line1 : line2 : _ -> "Weight: \n" <> show ( weight / (height * height))
      where height, weight :: Double
            height = read line1
            weight = read line2
    [] -> error "Assertion failure: empty input!"
