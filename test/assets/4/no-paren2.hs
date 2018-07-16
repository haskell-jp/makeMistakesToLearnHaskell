main = do
  input <- getContents
  putStr (unlines reverse (lines input))
