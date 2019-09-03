import qualified Data.Map.Strict    as M
import           System.Environment (getArgs)

data Result = Result
  { fruit       :: String
  , birthday    :: String
  , mailAddress :: String
  , prefecture  :: String
  }

fruitDictionary :: M.Map String String
fruitDictionary = M.fromList
  [ ("aki"      , "りんご")
  , ("asami"    , "みかん")
  , ("ouga"     , "バナナ")
  , ("kazuyo"   , "桃")
  , ("kazuo"    , "ブドウ")
  , ("kimiko"   , "りんご")
  , ("ken"      , "柿")
  , ("saki"     , "パイナップル")
  , ("shunsuke" , "ブドウ")
  , ("sousuke"  , "梨")
  , ("teppei"   , "ライチ")
  , ("natsumi"  , "みかん")
  , ("masashi"  , "桃")
  , ("meibi"    , "メロン")
  , ("yu"       , "グレープフルーツ")
  , ("yuji"     , "りんご")
  , ("yoshimasa", "バナナ")
  , ("rio"      , "バナナ")
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
  [ ("aki"      , "福岡県")
  , ("asami"    , "茨城県")
  , ("ouga"     , "栃木県")
  , ("kazuyo"   , "神奈川県")
  , ("kazuo"    , "神奈川県")
  , ("kimiaki"  , "岡山県")
  , ("kimiko"   , "群馬県")
  , ("ken"      , "福岡県")
  , ("saki"     , "福井県")
  , ("shunsuke" , "栃木県")
  , ("sousuke"  , "和歌山県")
  , ("teppei"   , "神奈川県")
  , ("natsumi"  , "栃木県")
  , ("masashi"  , "青森県")
  , ("meibi"    , "栃木県")
  , ("yu"       , "東京都")
  , ("yoshimasa", "三重県")
  , ("rio"      , "北海道")
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
      [name] -> do
        let mfavs = Result
              <$> M.lookup name fruitDictionary
              <*> M.lookup name birthdayDictionary
              <*> M.lookup name mailAddressDictionary
              <*> M.lookup name prefectureDictionary
        case mfavs of
            Just favs -> do
              putStrLn $ "fruit: " ++ fruit favs
              putStrLn $ "birthday: " ++ birthday favs
              putStrLn $ "mailAddress: " ++ mailAddress favs
              putStrLn $ "prefecture: " ++ prefecture favs
            Nothing ->
              putStrLn "Not found. He/She might be shy."
      _ -> error $  "Invalid arguments: " ++ show args
