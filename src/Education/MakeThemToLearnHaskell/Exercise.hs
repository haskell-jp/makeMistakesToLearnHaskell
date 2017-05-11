{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeThemToLearnHaskell.Exercise
  ( Exercise(verify)
  , ExerciseId
  , Result(..)
  , Details
  , loadHeaders
  , loadDescriptionById
  , loadExampleSolution
  , loadLastShown
  , saveLastShownId
  ) where


#include <imports/external.hs>

import qualified Paths_makeThemToLearnHaskell

import           Education.MakeThemToLearnHaskell.Exercise.Record
import           Education.MakeThemToLearnHaskell.Exercise.Types
import qualified Education.MakeThemToLearnHaskell.Exercise.Evaluator.RunHaskell as RunHaskell
import           Education.MakeThemToLearnHaskell.Util


exercises :: Vector Exercise
exercises = Vector.fromList [exercise1]
  where
    exercise1 =
      Exercise "1" $ \prgFile -> do
        result <- RunHaskell.runFile prgFile
        case result of
            Right (out, errB) -> do
              let right = "Hello, world!\n"
                  msg = Text.unlines
                        [ Text.replicate 80 "="
                        , "Your program's output: " <> Text.pack (show out)
                        , "      Expected output: " <> Text.pack (show right)
                        ]
                  err = TextEncoding.decodeUtf8 errB
                  eMsg =
                    if Text.null err
                      then ""
                      else
                        Text.unlines
                          ["Found error message printed on stderr:", err]
              return $
                if out == right && Text.null eMsg
                  then Success $ "Nice output!\n\n" <> msg
                  else Fail $ "Wrong output!\n\n" <> msg <> eMsg
           -- TODO: return Fail when copile error.
            Left err -> return $ Error $ Text.pack $ show err



loadHeaders :: IO [Text]
loadHeaders = mapM loadHeader $ Vector.toList exercises
  where
    loadHeader ex = extractHeader ex =<< loadDescription ex
    extractHeader ex desc =
      dieWhenNothing ("The description of exercise '" ++ exerciseName ex ++ "' is empty!")
        $ cutHash <$> headMay (Text.lines desc)
    cutHash h =
      Text.strip $ fromMaybe h $ Text.stripPrefix "# " h


loadDescription :: Exercise -> IO Text
loadDescription = loadWithExtension ".md"


loadExampleSolution :: Exercise -> IO Text
loadExampleSolution = loadWithExtension ".hs"


loadWithExtension :: String -> Exercise -> IO Text
loadWithExtension ext ex =
  Paths_makeThemToLearnHaskell.getDataFileName ("assets/" ++ exerciseName ex ++ ext)
    >>= Text.readFile


loadDescriptionById :: ExerciseId -> IO (Maybe Text)
loadDescriptionById n = MaybeT.runMaybeT $ do
  ex <- Error.hoistMaybe $ getById n
  liftIO $ loadDescription ex


-- Handle error internally.
-- Because lastShownId is usually saved internally.
loadLastShown :: IO Exercise
loadLastShown =
  loadLastShownId >>=
    dieWhenNothing "Assertion failure: Invalid lastShownId saved! " . getById


getById :: ExerciseId -> Maybe Exercise
getById n = exercises !? (n - 1)
