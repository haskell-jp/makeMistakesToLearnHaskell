data Entry = Entry
  { category :: String
  , price :: Integer
  } deriving Show

main = do
  cat <- getLine
  priceStr <- getLine
  print (Entry cat (read priceStr))
