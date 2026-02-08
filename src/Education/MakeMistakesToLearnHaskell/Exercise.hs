{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise
  ( Exercise(verify, name)
  , Name
  , Result(..)
  , Details
  , loadHeaders
  , loadDescriptionByName
  , loadExampleSolution
  , loadLastShown
  , saveLastShownName
  , unsafeGetByName
  ) where


#include <imports/external.hs>
#include <imports/io.hs>

import           Education.MakeMistakesToLearnHaskell.Exercise.Core
import           Education.MakeMistakesToLearnHaskell.Env
import           Education.MakeMistakesToLearnHaskell.Evaluator.Regex
import           Education.MakeMistakesToLearnHaskell.Evaluator.Types
import           Education.MakeMistakesToLearnHaskell.Exercise.FormatMessage
import           Education.MakeMistakesToLearnHaskell.Exercise.Record
import           Education.MakeMistakesToLearnHaskell.Exercise.Types
import           Education.MakeMistakesToLearnHaskell.Error
import           Education.MakeMistakesToLearnHaskell.Text

import Education.MakeMistakesToLearnHaskell.Exercise.Ex01
import Education.MakeMistakesToLearnHaskell.Exercise.Ex02
import Education.MakeMistakesToLearnHaskell.Exercise.Ex02_5
import Education.MakeMistakesToLearnHaskell.Exercise.Ex03
import Education.MakeMistakesToLearnHaskell.Exercise.Ex04
import Education.MakeMistakesToLearnHaskell.Exercise.Ex05
import Education.MakeMistakesToLearnHaskell.Exercise.Ex06
import Education.MakeMistakesToLearnHaskell.Exercise.Ex07
import Education.MakeMistakesToLearnHaskell.Exercise.Ex08
import Education.MakeMistakesToLearnHaskell.Exercise.Ex09
import Education.MakeMistakesToLearnHaskell.Exercise.Ex10
import Education.MakeMistakesToLearnHaskell.Exercise.Ex11
import Education.MakeMistakesToLearnHaskell.Exercise.Ex12
import Education.MakeMistakesToLearnHaskell.Exercise.Ex13
import Education.MakeMistakesToLearnHaskell.Exercise.Ex14
import Education.MakeMistakesToLearnHaskell.Exercise.Ex15
import Education.MakeMistakesToLearnHaskell.Exercise.Ex16
import Education.MakeMistakesToLearnHaskell.Exercise.Ex16_5
import Education.MakeMistakesToLearnHaskell.Exercise.Ex17
import Education.MakeMistakesToLearnHaskell.Exercise.Ex18
import Education.MakeMistakesToLearnHaskell.Exercise.Ex19
import Education.MakeMistakesToLearnHaskell.Exercise.Ex20
import Education.MakeMistakesToLearnHaskell.Exercise.Ex21
import Education.MakeMistakesToLearnHaskell.Exercise.Ex22

exercises :: [Exercise]
exercises =
  [ exercise1
  , exercise2
  , exercise2_5
  , exercise3
  , exercise4
  , exercise5
  , exercise6
  , exercise7
  , exercise8
  , exercise9
  , exercise10
  , exercise11
  , exercise12
  , exercise13
  , exercise14
  , exercise15
  , exercise16
  , exercise16_5
  , exercise17
  , exercise18
  , exercise19
  , exercise20
  , exercise21
  , exercise22
  ]

loadHeaders :: IO [Text]
loadHeaders = mapM loadHeader exercises
  where
    loadHeader ex = extractHeader ex =<< loadDescription ex

    extractHeader ex desc =
      dieWhenNothing ("The description of exercise '" ++ name ex ++ "' is empty!")
        $ appendName ex . cutHash <$> headMay (Text.lines desc)

    cutHash h =
      Text.strip $ fromMaybe h $ Text.stripPrefix "# " h

    appendName ex h =
      Text.pack (name ex) <> ": " <> h


loadDescription :: Exercise -> IO Text
loadDescription = loadWithExtension ".md"


loadExampleSolution :: Exercise -> IO Text
loadExampleSolution = loadWithExtension ".hs"


loadWithExtension :: String -> Exercise -> IO Text
loadWithExtension ext ex =
  Paths.getDataFileName ("assets/" ++ name ex ++ ext)
    >>= readUtf8File


loadDescriptionByName :: Name -> IO (Maybe Text)
loadDescriptionByName n = MaybeT.runMaybeT $ do
  ex <- Error.hoistMaybe $ getByName n
  liftIO $ loadDescription ex


-- Handle error internally.
-- Because lastShownName is usually saved internally.
loadLastShown :: Env -> IO Exercise
loadLastShown e =
  loadLastShownName e >>=
    dieWhenNothing "Assertion failure: Invalid lastShownName saved! " . getByName


getByName :: Name -> Maybe Exercise
getByName n = List.find ((== n) . name) exercises


unsafeGetByName :: Name -> Exercise
unsafeGetByName = fromMaybe (error "unsafeGetByName: No exercise found!") . getByName
