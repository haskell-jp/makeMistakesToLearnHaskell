import System.Environment (getArgs)
import System.IO (BufferMode(..), hSetBuffering, stdout)


main :: IO ()
main = do
  args <- getArgs
  case args of
      [metsStr, weightStr, minutesStr] -> do
        let mets = read metsStr
            weight = read weightStr
            minutes = read minutesStr
        print (mets * weight * (minutes / 60) * 1.05)
      _ ->
        error ("Invalid input: " ++ show args)
