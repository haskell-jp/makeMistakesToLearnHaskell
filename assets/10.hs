main = do
  putStrLn "Height Weight: "
  hwStr <- getLine
  case words hwStr of
    [heightStr, weightStr] ->
      print (bmiFromStrings heightStr weightStr)
    [heightStr] -> do
      putStrLn "Weight: "
      weightStr <- getLine
      print (bmiFromStrings heightStr weightStr)
    other ->
      putStrLn ("Invalid input: " ++ hwStr)


bmiFromStrings :: String -> String -> Double
bmiFromStrings hwStr weightStr =
  let height = read hwStr
      weight = read weightStr
  in weight / (height * height)
