main = do
  line1 <- getLine
  let principal = read line1

  line2 <- getLine
  let interestRate = read line2

  line3 <- getLine
  let years = read line3

  print (principal * (1 + interestRate / 100) ^ years)
