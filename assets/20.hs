main :: IO ()
main =
  print =<< (yearlyRate <$> (read <$> getLine) <*> (read <$> getLine) <*> (read <$> getLine))


-- Ref: https://support.microsoft.com/ja-jp/help/141695/xl-how-to-calculate-compound-interest
yearlyRate :: Double -> Double -> Integer -> Double
yearlyRate principal interestRate years =
  principal * (1 + interestRate / 100) ^ years
