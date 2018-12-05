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

import           Education.MakeMistakesToLearnHaskell.Exercise.Core
import           Education.MakeMistakesToLearnHaskell.Diagnosis
import           Education.MakeMistakesToLearnHaskell.Env
import           Education.MakeMistakesToLearnHaskell.Evaluator.Regex
import qualified Education.MakeMistakesToLearnHaskell.Evaluator.RunHaskell as RunHaskell
import           Education.MakeMistakesToLearnHaskell.Evaluator.Types
import           Education.MakeMistakesToLearnHaskell.Exercise.FormatMessage
import           Education.MakeMistakesToLearnHaskell.Exercise.Record
import           Education.MakeMistakesToLearnHaskell.Exercise.Types
import           Education.MakeMistakesToLearnHaskell.Error
import           Education.MakeMistakesToLearnHaskell.Text

-- 課題
import Education.MakeMistakesToLearnHaskell.Exercise.Ex01
import Education.MakeMistakesToLearnHaskell.Exercise.Ex02
import Education.MakeMistakesToLearnHaskell.Exercise.Ex02_5
import Education.MakeMistakesToLearnHaskell.Exercise.Ex03
import Education.MakeMistakesToLearnHaskell.Exercise.Ex04
import Education.MakeMistakesToLearnHaskell.Exercise.Ex05

exercises :: [(Name, Exercise)]
exercises = map (\e -> (name e, e))
  [ exercise1
  , exercise2
  , exercise2_5
  , exercise3
  , exercise4
  , exercise5
  ]

loadHeaders :: IO [Text]
loadHeaders = mapM (loadHeader . snd) exercises
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
getByName n = lookup n exercises


unsafeGetByName :: Name -> Exercise
unsafeGetByName = fromMaybe (error "unsafeGetByName: No exercise found!") . getByName
