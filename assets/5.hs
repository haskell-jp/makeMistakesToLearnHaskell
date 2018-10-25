main = do
  principal <- getLine
  interestRate <- getLine
  years <- getLine
  print (read principal * (1 + read interestRate / 100) ^ read years)
