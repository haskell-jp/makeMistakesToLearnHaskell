{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Exception                                     (throwIO)
import qualified Data.Text                                             as T
import qualified Data.Text.IO                                          as TI
import           GHC.Generics                                          (Generic)
import           Options.Generic                                       (ParseRecord,
                                                                        getRecord)

import qualified Education.MakeMistakesToLearnHaskell.Commons.Exercise as Exercise
import           Education.MakeMistakesToLearnHaskell.Report.Client    (Report (..),
                                                                        postReport,
                                                                        reportUrl)


data Args = Args
  { endpoint :: !String
  , name     :: !String
  , answer   :: !T.Text
  , details  :: !T.Text
  } deriving (Generic, Show)

instance ParseRecord Args


main :: IO ()
main = do
  args <- getRecord "Post report"
  let r = Report
        { exerciseName = name args
        , exerciseAnswer = answer args
        , exerciseFailBy = Exercise.WrongOutput $ details args
        }
  TI.putStrLn . reportUrl =<< either throwIO return =<< postReport (endpoint args) r
