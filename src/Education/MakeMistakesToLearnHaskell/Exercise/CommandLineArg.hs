{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.CommandLineArg
  ( asMereString
  , assertMereString
  , writePathsIn
  , isFilePath
  ) where

#include <imports/external.hs>
#include <imports/io.hs>

import           Education.MakeMistakesToLearnHaskell.Exercise.Types


writePathsIn :: FilePath -> [CommandLineArg] -> IO ()
writePathsIn dir args = do
  Dir.createDirectoryIfMissing False $ dir </> "CommandLineArg"
  mapM_ writePath args
 where
  writePath (Mere _) = return ()
  writePath (FilePath path content) =
    writeUtf8File (dir </> path) content


asMereString :: CommandLineArg -> String
asMereString (Mere s) = s
asMereString (FilePath path _content) = path


assertMereString :: CommandLineArg -> String
assertMereString (Mere s) = s
assertMereString (FilePath path _content) = error $ "Assertion failure: unexpected FilePath argument: " ++ show path


isFilePath :: CommandLineArg -> Bool
isFilePath (Mere _) = False
isFilePath (FilePath _path _content) = True
