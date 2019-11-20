{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Record
  ( loadLastShownName
  , saveLastShownName
  ) where

#include <imports/external.hs>

import           Education.MakeMistakesToLearnHaskell.Env
import           Education.MakeMistakesToLearnHaskell.Exercise.Types
import           Education.MakeMistakesToLearnHaskell.Error


loadLastShownName :: Env -> IO Name
loadLastShownName e = do
  path <- prepareRecordFilePath e
  exists <- Dir.doesFileExist path
  if exists then
    fmap (lastShownName . read) $ readFile path
  else
    return "1"


saveLastShownName :: Env -> Name -> IO ()
saveLastShownName e n = do
  path <- prepareRecordFilePath e
  writeFile path $ show $ Record n


prepareRecordFilePath :: Env -> IO FilePath
prepareRecordFilePath e = do
  let d = appHomePath e </> dirName
  Dir.createDirectoryIfMissing True d
  return $ d </> "record.txt"


dirName :: FilePath
dirName = "Exercise"
