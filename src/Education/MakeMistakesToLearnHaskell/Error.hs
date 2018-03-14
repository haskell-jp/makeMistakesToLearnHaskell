{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Error  where


#include <imports/external.hs>


import           Education.MakeMistakesToLearnHaskell.Env (appName)


dieWhenNothing :: String -> Maybe a -> IO a
dieWhenNothing _ (Just x) = return x
dieWhenNothing msg _ = die msg


die :: String -> IO a
die msg = Exit.die $ appName ++ ": ERROR: " ++ msg


throwWhenLeft :: Exception e => Either e a -> IO a
throwWhenLeft = either throwIO return
