data Entry = Entry
  { category :: String
  , price :: Integer
  }

main = do
  cat <- getLine
  priceStr <- getLine
  let entry = Entry cat (read priceStr)
  putStrLn ("Category: " ++ category entry)
  putStrLn ("Price: " ++ show (price entry))
