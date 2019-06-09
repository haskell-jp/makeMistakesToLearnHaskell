{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Exception                                     (throwIO)
import           Control.Monad.IO.Class                                (liftIO)
import qualified Data.Text.Lazy                                        as T
import qualified Data.Text.Lazy.IO                                     as TI
import           GHC.Generics                                          (Generic)
import           Network.HTTP.Client                                   (defaultManagerSettings,
                                                                        newManager)
-- import           Servant.API
import           Options.Generic                                       (ParseRecord,
                                                                        getRecord)
import           Servant.Client                                        (ClientM,
                                                                        client,
                                                                        mkClientEnv,
                                                                        parseBaseUrl,
                                                                        runClientM)

import qualified Education.MakeMistakesToLearnHaskell.Commons.Exercise as Exercise
import           Education.MakeMistakesToLearnHaskell.Report.Server    (Report (..),
                                                                        Result,
                                                                        api,
                                                                        reportUrl)


data Args = Args
  { endpoint :: !String
  , name     :: !String
  , answer   :: !T.Text
  , details  :: !T.Text
  } deriving (Generic, Show)

instance ParseRecord Args

postReport :: Report -> ClientM Result
postReport = client api


main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  args <- getRecord "Post report"
  env <- mkClientEnv manager <$> parseBaseUrl (endpoint args)
  let r = Report
        { exerciseName = name args
        , exerciseAnswer = answer args
        , exerciseFailBy = Exercise.WrongOutput $ details args
        }
  either throwIO return =<< runClientM (liftIO . TI.putStrLn . reportUrl =<< postReport r) env
