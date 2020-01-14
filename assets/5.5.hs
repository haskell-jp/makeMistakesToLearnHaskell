main :: IO ()
main = do
  line <- getLine
  let num = read line
  if num >= 100 then
    putStrLn "Extremely big!!"
  else if num >= 10 then
    putStrLn "Very big!"
  else
    print num
