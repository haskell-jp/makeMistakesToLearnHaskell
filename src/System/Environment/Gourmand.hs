{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Environment.Gourmand
  ( runGourmand
  , runGourmandEither
  , Source(..)
  ) where


-- TODO: Generalize by ExistentialQuantification?
data Source = 
  CmdArgs CmdArgsPolicy
    | KVFile KVFileReader FilePath


data CmdArgsPolicy = 
  UntilDashDash | UntilFirstNonOption


newtype Gourmand a =
  Gourmand { unGourmand :: (StateT Args (ReaderT KVs (ExceptT GourmandError IO))) a } deriving (MonadIO)


type Args = [String]


type KVs =
  HashMap Key KVsEntry


type Key = ByteString -- Short ByteString?


data KVsEntry =


runGourmandEither :: [Source] -> Gourmand a -> IO (Either GourmandError a)
runGourmandEither srcs g =
  error "runGourmand is not defined yet!"


runGourmand :: Source s => [s] -> Gourmand a -> IO a
runGourmand srcs g =
  error "runGourmand is not defined yet!"
