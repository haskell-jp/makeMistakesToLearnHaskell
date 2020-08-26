{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.CommandLineArg
  ( asMereString
  , assertMereString
  , assertFilePath
  , writePathsIn
  , isFilePath
  ) where

#include <imports/external.hs>
#include <imports/io.hs>

import           Education.MakeMistakesToLearnHaskell.Exercise.Types


writePathsIn :: FilePath -> [CommandLineArg] -> IO ()
writePathsIn dir = mapM_ writePath
 where
  writePath (Mere _) = return ()
  writePath (FilePath path content) =
    writeUtf8File (dir </> path) content


asMereString :: CommandLineArg -> String
asMereString (Mere s) = s
asMereString (FilePath path _content) = path


assertMereString :: HasCallStack => CommandLineArg -> String
assertMereString (Mere s) = s
assertMereString (FilePath path _content) = error $ "Assertion failure: unexpected FilePath argument: " ++ show path


assertFilePath :: HasCallStack => CommandLineArg -> String
assertFilePath (Mere s) = error $ "Assertion failure: unexpected Mere argument: " ++ show s
assertFilePath (FilePath path _content) = path


isFilePath :: CommandLineArg -> Bool
isFilePath (Mere _) = False
isFilePath (FilePath _path _content) = True
