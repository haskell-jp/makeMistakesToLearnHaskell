main = do
  input <- getContents
  putStrLn (unlines (reverse (lines input)))
