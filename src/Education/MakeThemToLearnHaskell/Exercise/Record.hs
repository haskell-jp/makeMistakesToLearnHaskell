{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeThemToLearnHaskell.Exercise.Record
  ( loadLastShownId
  , saveLastShownId
  ) where

#include <imports/external.hs>

import           Education.MakeThemToLearnHaskell.Env
import           Education.MakeThemToLearnHaskell.Exercise.Types
import           Education.MakeThemToLearnHaskell.Util


loadLastShownId :: Env -> IO ExerciseId
loadLastShownId e = do
  path <- prepareRecordFilePath e
  exists <- Dir.doesFileExist path
  if exists
    then
      lastShownId <$> (throwWhenLeft =<< Yaml.decodeFileEither path)
    else
      return 1


saveLastShownId :: Env -> ExerciseId -> IO ()
saveLastShownId e n = do
  path <- prepareRecordFilePath e
  Yaml.encodeFile path $ Record n


prepareRecordFilePath :: Env -> IO FilePath
prepareRecordFilePath e = do
  let d = appHomePath e </> dirName
  Dir.createDirectoryIfMissing True d
  return $ d </> "record.yaml"


dirName :: FilePath
dirName = "Exercise"
