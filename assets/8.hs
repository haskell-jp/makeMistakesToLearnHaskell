main = do
  numeratorStr <- getLine
  let numerator = read numeratorStr :: Integer

  denominatorStr <- getLine
  let denominator = read denominatorStr :: Integer

  let (divResult, modResult) = divMod numerator denominator
  let (quotResult, remResult) = quotRem numerator denominator

  putStrLn ("div: " ++ show divResult)
  putStrLn ("mod: " ++ show modResult)
  putStrLn ("quot: " ++ show quotResult)
  putStrLn ("rem: " ++ show remResult)
