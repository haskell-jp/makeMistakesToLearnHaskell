import qualified Data.Map.Strict    as M
import           System.Environment (getArgs)
import           System.IO (stderr, hPutStrLn)

fruitDictionary :: M.Map String String
fruitDictionary = M.fromList
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

birthdayDictionary :: M.Map String String
birthdayDictionary = M.fromList
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

mailAddressDictionary :: M.Map String String
mailAddressDictionary = M.fromList
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

prefectureDictionary :: M.Map String String
prefectureDictionary = M.fromList
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

main :: IO ()
main = do
  args <- getArgs
  case args of
      [name] -> do
        let mResult = formatResult
              <$> M.lookup name fruitDictionary
              <*> M.lookup name birthdayDictionary
              <*> M.lookup name mailAddressDictionary
              <*> M.lookup name prefectureDictionary
        case mResult of
            Just result -> putStr result
            Nothing     -> hPutStrLn stderr "Not found. He/She might be shy."
      _ -> error $ "Invalid arguments: " ++ show args

formatResult :: String -> String -> String -> String -> String
formatResult fruit birthday mailAddress prefecture = unlines
  [ "fruit: " ++ fruit
  , "birthday: " ++ birthday
  , "mailAddress: " ++ mailAddress
  , "prefecture: " ++ prefecture
  ]
