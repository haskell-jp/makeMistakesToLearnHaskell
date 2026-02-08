main = do
  putStrLn "Height Weight: "
  hwStr <- getLine
  case words hwStr of
    heightStr : weightStr : _ ->
      print (bmiFromStrings heightStr weightStr)
    [heightStr] -> do
      weightStr <- askWeight
      print (bmiFromStrings heightStr weightStr)
    _ -> do
      putStrLn "Invalid input"
      main


bmiFromStrings :: String -> String -> Double
bmiFromStrings hwStr weightStr =
  let height = read hwStr
      weight = read weightStr
   in weight / (height * height)


askWeight = do
  putStrLn "Weight: "
  ans <- getLine
  case words ans of
      weightStr : _ ->
        return weightStr
      _ -> do
        putStrLn "Invalid input"
        askWeight
