main = do
  putStrLn "Height Weight: "
  hwStr <- getLine
  case words hwStr of
    heightStr : weightStr : _ -> do
      let height = read heightStr
          weight = read weightStr
      print (weight / (height * height))
    [heightStr] -> do
      putStrLn "Weight: "
      weightStr <- getLine
      let height = read heightStr
          weight = read weightStr
      print (weight / (height * height))
    other ->
      putStrLn ("Invalid input: " ++ hwStr)
