{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Ex21
  ( exercise21
  , argsGenerator
  , stdinGenerator
  , formatResult
  , idDictionary
  , fruitDictionary
  , birthdayDictionary
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
      . map show
      . Map.keys
      $ idDictionary
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
        let mResult = do
              personId <- Map.lookup (Text.pack nameString) idDictionary
              formatResult
                <$> Map.lookup personId fruitDictionary
                <*> Map.lookup personId birthdayDictionary
        case mResult of
            Just result -> Right result
            Nothing     -> Right "Not found. He/She might be shy.\n"
      _ -> Left $ "Invalid arguments: " <> Text.pack (show mereStrings)
 where
  mereStrings = map assertMereString args


formatResult :: Text -> Text -> Text
formatResult fruit birthday = Text.unlines
  [ "fruit: " <> fruit
  , "birthday: " <> birthday
  ]


idDictionary :: Map Text Integer
idDictionary = Map.fromList
  [ ("aki"       ,  1)
  , ("asami"     ,  2)
  , ("kazuo"     ,  3)
  , ("kazuyo"    ,  4)
  , ("ken"       ,  5)
  , ("kimiaki"   ,  6)
  , ("kimiko"    ,  7)
  , ("masashi"   ,  8)
  , ("meibi"     ,  9)
  , ("natsumi"   , 10)
  , ("ouga"      , 11)
  , ("rio"       , 12)
  , ("saki"      , 13)
  , ("shunsuke"  , 14)
  , ("sousuke"   , 15)
  , ("teppei"    , 16)
  , ("yoshimasa" , 17)
  , ("yu"        , 18)
  , ("yuji"      , 19)
  ]


fruitDictionary :: Map Integer Text
fruitDictionary = Map.fromList
  [ ( 1, "apple")
  , ( 2, "orange")
  , ( 3, "grape")
  , ( 4, "peach")
  , ( 5, "kaki")
  , ( 7, "apple")
  , ( 8, "peach")
  , ( 9, "melon")
  , (10, "orange")
  , (11, "banana")
  , (12, "banana")
  , (13, "dragon fruit")
  , (14, "grape")
  , (15, "pear")
  , (16, "liche")
  , (17, "banana")
  , (18, "grape fruit")
  , (19, "apple")
  ]


birthdayDictionary :: Map Integer Text
birthdayDictionary = Map.fromList
  [ ( 1, "1968-01-18")
  , ( 2, "2005-06-05")
  , ( 3, "1942-11-04")
  , ( 4, "1952-04-26")
  , ( 6, "2000-05-17")
  , ( 7, "1988-07-31")
  , ( 8, "1954-01-28")
  , ( 9, "1953-11-07")
  , (10, "1988-04-16")
  , (11, "1995-09-14")
  , (12, "1987-12-08")
  , (13, "2004-05-10")
  , (14, "1960-11-15")
  , (15, "1977-01-01")
  , (16, "1976-12-04")
  , (17, "1992-12-06")
  , (18, "1990-06-22")
  , (19, "1989-04-16")
  ]
