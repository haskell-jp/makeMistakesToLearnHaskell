import           Control.Error.Util (note)
import qualified Data.Map.Strict    as M
import           System.Environment (getArgs)

idDictionary :: M.Map String Integer
idDictionary = M.fromList
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

fruitDictionary :: M.Map Integer String
fruitDictionary = M.fromList
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

birthdayDictionary :: M.Map Integer String
birthdayDictionary = M.fromList
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

main :: IO ()
main = do
  args <- getArgs
  case args of
      [name] -> do
        let eResult = do
              personId <- note
                "ID not found. Who is that person?"
                (M.lookup name idDictionary)
              formatResult
                <$> note
                      "Fruit not found. He/She might like vegitables better."
                      (M.lookup personId fruitDictionary)
                <*> note
                      "Birthday not found. He/She might want to hide his/her age."
                      (M.lookup personId birthdayDictionary)
        case eResult of
            Right result -> putStr result
            Left emsg    -> putStrLn emsg
      _ -> error $ "Invalid arguments: " ++ show args

formatResult :: String -> String -> String
formatResult fruit birthday = unlines
  [ "fruit: " ++ fruit
  , "birthday: " ++ birthday
  ]
