import qualified Data.Map.Strict    as M
import           System.Environment (getArgs)

data Result = Result
  { fruit       :: String
  , birthday    :: String
  , mailAddress :: String
  , prefecture  :: String
  }

fruitsDictionary :: M.Map String String
fruitsDictionary =
  error "favoritesDictionary is not defined yet!"

birthdayDictionary :: M.Map String String
birthdayDictionary =
  error "favoritesDictionary is not defined yet!"

mailAddressDictionary :: M.Map String String
mailAddressDictionary =
  error "favoritesDictionary is not defined yet!"

prefectureDictionary :: M.Map String String
prefectureDictionary =
  error "favoritesDictionary is not defined yet!"

main :: IO ()
main = do
  args <- getArgs
  case args of
      [name] ->
        Favorites <$> lookup fruitsDictionary
      _ -> putStrLn "Not found."
