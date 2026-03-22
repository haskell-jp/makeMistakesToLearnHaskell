main :: IO ()
main = do
  result <- sumEntries 0
  print result

sumEntries currentSum = do
  line <- getLine
  case words line of
      [_cat, priceStr] ->
        sumEntries (currentSum + read priceStr)
      [] ->
        return currentSum
      _ ->
        error ("Invalid input: " ++ line)
