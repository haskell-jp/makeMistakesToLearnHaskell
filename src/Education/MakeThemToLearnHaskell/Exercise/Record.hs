{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeThemToLearnHaskell.Exercise.Record
  ( loadLastShownId
  , saveLastShownId
  ) where

#include <imports/external.hs>

import           Education.MakeThemToLearnHaskell.Exercise.Types
import           Education.MakeThemToLearnHaskell.Util


loadLastShownId :: IO ExerciseId
loadLastShownId = do
  path <- getRecordFilePath
  exists <- Dir.doesFileExist path
  if exists
    then
      lastShownId <$> (throwWhenLeft =<< Yaml.decodeFileEither path)
    else
      return 1


saveLastShownId :: ExerciseId -> IO ()
saveLastShownId n = do
  path <- getRecordFilePath
  Yaml.encodeFile path $ Record n


getRecordFilePath :: IO FilePath
getRecordFilePath = do
  d <- (</> dirName) <$> Dir.getXdgDirectory Dir.XdgData appName
  Dir.createDirectoryIfMissing True d
  return $ d </> "record.yaml"


dirName :: FilePath
dirName = "Exercise"
