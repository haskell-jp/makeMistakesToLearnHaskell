main =
  input <- getContents
  putStr (unlines (reverse (lines input)))
