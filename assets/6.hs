data Entry = Entry
  { category :: String
  , price :: Integer
  }

main = do
  cat <- getLine
  priceStr <- getLine
  let entry = Entry { category = cat, price = read priceStr }
  putStrLn ("Category: " ++ category entry)
  putStrLn ("Price: " ++ show (price entry))
