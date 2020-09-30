{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex21
  ( exercise21
  , argsGenerator
  , stdinGenerator
  , formatResult
  , fruitDictionary
  , birthdayDictionary
  , mailAddressDictionary
  , prefectureDictionary
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.CommandLineArg
import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types


exercise21 :: Exercise
exercise21 = Exercise "21"
           $ runHaskellExerciseWithArgsAndStdin diag (judgeWithException answer) argsGenerator stdinGenerator


diag :: Diagnosis
diag _code _msg = "" -- TODO: Not implemented


argsGenerator :: Gen [CommandLineArg]
argsGenerator = QuickCheck.frequency [(4, args1Valid), (1, args1Invalid), (1, argsMore)]
 where
  -- NOTE: On Windows, passing an empty string in command line arguments is hard.
  nonEmptyString =
    fmap Mere
      . QuickCheck.listOf1
      . QuickCheck.elements
      $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

  args1Valid =
    fmap ((: []) . Mere)
      . QuickCheck.elements
      . Set.toList
      $ Set.unions [fruitDictionaryKeys, birthdayDictionaryKeys, mailAddressDictionaryKeys, prefectureDictionaryKeys]
  args1Invalid = (: []) <$> nonEmptyString

  argsMore = do
    args2 <- QuickCheck.vectorOf 2 nonEmptyString
    leftArgs <- QuickCheck.listOf nonEmptyString
    return $ args2 ++ leftArgs


stdinGenerator :: Gen Text
stdinGenerator = pure ""


answer :: [CommandLineArg] -> Text -> Either Text Text
answer args _input =
  case mereStrings of
      [nameString] -> do
        let mResult = formatResult
              <$> Map.lookup nameString fruitDictionary
              <*> Map.lookup nameString birthdayDictionary
              <*> Map.lookup nameString mailAddressDictionary
              <*> Map.lookup nameString prefectureDictionary
        case mResult of
            Just result -> Right result
            Nothing     -> Right "Not found. He/She might be shy.\n"
      _ -> Left $ "Invalid arguments: " <> Text.pack (show mereStrings)
 where
  mereStrings = map assertMereString args


formatResult :: Text -> Text -> Text -> Text -> Text
formatResult fruit birthday mailAddress prefecture = Text.unlines
  [ "fruit: " <> fruit
  , "birthday: " <> birthday
  , "mailAddress: " <> mailAddress
  , "prefecture: " <> prefecture
  ]


fruitDictionary :: Map String Text
fruitDictionary = Map.fromList
  [ ("aki"      , "apple")
  , ("asami"    , "orange")
  , ("ouga"     , "banana")
  , ("kazuyo"   , "peach")
  , ("kazuo"    , "grape")
  , ("kimiko"   , "apple")
  , ("ken"      , "kaki")
  , ("saki"     , "pineapple")
  , ("shunsuke" , "grape")
  , ("sousuke"  , "pear")
  , ("teppei"   , "liche")
  , ("natsumi"  , "orange")
  , ("masashi"  , "peach")
  , ("meibi"    , "melon")
  , ("yu"       , "grape fruit")
  , ("yuji"     , "apple")
  , ("yoshimasa", "banana")
  , ("rio"      , "banana")
  ]

fruitDictionaryKeys :: Set String
fruitDictionaryKeys = Map.keysSet fruitDictionary


birthdayDictionary :: Map String Text
birthdayDictionary = Map.fromList
  [ ("aki"      , "1968/1/18")
  , ("asami"    , "1995/6/5")
  , ("ouga"     , "1995/9/14")
  , ("kazuyo"   , "1952/4/26")
  , ("kazuo"    , "1942/11/4")
  , ("kimiaki"  , "1970/5/17")
  , ("kimiko"   , "1988/7/31")
  , ("saki"     , "1990/3/26")
  , ("shunsuke" , "1960/11/15")
  , ("sousuke"  , "1977/1/1")
  , ("teppei"   , "1976/12/4")
  , ("natsumi"  , "1988/4/16")
  , ("masashi"  , "1954/1/28")
  , ("meibi"    , "1953/11/7")
  , ("yu"       , "1990/6/22")
  , ("yuji"     , "1989/4/16")
  , ("yoshimasa", "1992/12/6")
  , ("rio"      , "1987/12/8")
  ]

birthdayDictionaryKeys :: Set String
birthdayDictionaryKeys = Map.keysSet birthdayDictionary


mailAddressDictionary :: Map String Text
mailAddressDictionary = Map.fromList
  [ ("aki"      , "yamane_aki@example.com")
  , ("asami"    , "ooizumi_asami@example.com")
  , ("ouga"     , "nasu_ouga@example.com")
  , ("kazuyo"   , "ezaki_kazuyo@example.com")
  , ("kimiaki"  , "tsutsui_kimiaki@example.com")
  , ("kimiko"   , "maeda_kimiko@example.com")
  , ("ken"      , "mita_ken@example.com")
  , ("saki"     , "okudera_saki@example.com")
  , ("shunsuke" , "konuma_shunsuke@example.com")
  , ("sousuke"  , "asari_sousuke@example.com")
  , ("teppei"   , "taniguchi_teppei@example.com")
  , ("natsumi"  , "komachi_natsumi@example.com")
  , ("masashi"  , "itano_masashi@example.com")
  , ("meibi"    , "shimizu_meibi@example.com")
  , ("yu"       , "matsushima_yu@example.com")
  , ("yoshimasa", "onoda_yoshimasa@example.com")
  , ("rio"      , "tsukahara_rio@example.com")
  ]

mailAddressDictionaryKeys :: Set String
mailAddressDictionaryKeys = Map.keysSet mailAddressDictionary


prefectureDictionary :: Map String Text
prefectureDictionary = Map.fromList
  [ ("aki"      , "Fukuoka")
  , ("asami"    , "Ibaraki")
  , ("ouga"     , "Tochigi")
  , ("kazuo"    , "Kanagawa")
  , ("kimiaki"  , "Okayama")
  , ("kimiko"   , "Gunma")
  , ("ken"      , "Fukuoka")
  , ("saki"     , "Fukui")
  , ("shunsuke" , "Tochigi")
  , ("sousuke"  , "Wakayama")
  , ("teppei"   , "Kanagawa")
  , ("natsumi"  , "Tochigi")
  , ("masashi"  , "Aomori")
  , ("meibi"    , "Tochigi")
  , ("yu"       , "Tokyo")
  , ("yoshimasa", "Mie")
  , ("rio"      , "Hokkaido")
  ]

prefectureDictionaryKeys :: Set String
prefectureDictionaryKeys = Map.keysSet prefectureDictionary
