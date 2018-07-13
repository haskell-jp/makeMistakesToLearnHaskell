{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise
  ( Exercise(verify)
  , ExerciseId
  , Result(..)
  , Details
  , loadHeaders
  , loadDescriptionById
  , loadExampleSolution
  , loadLastShown
  , saveLastShownId
  , unsafeGetById
  ) where


#include <imports/external.hs>

import qualified Paths_makeMistakesToLearnHaskell

import           Education.MakeMistakesToLearnHaskell.Diagnosis
import           Education.MakeMistakesToLearnHaskell.Env
import qualified Education.MakeMistakesToLearnHaskell.Evaluator.RunHaskell as RunHaskell
import           Education.MakeMistakesToLearnHaskell.Evaluator.Types
import           Education.MakeMistakesToLearnHaskell.Exercise.Record
import           Education.MakeMistakesToLearnHaskell.Exercise.Types
import           Education.MakeMistakesToLearnHaskell.Error
import           Education.MakeMistakesToLearnHaskell.Text

import           Debug.NoTrace


exercises :: Vector Exercise
exercises = Vector.fromList [exercise1, exercise2, exercise3]
  where
    exercise1 =
      Exercise "1" $ runHaskellExercise diag1 "Hello, world!\n"

    diag1 :: Diagnosis
    diag1 code msg
      | "parse error on input" `Text.isInfixOf` msg
          && "'" `Text.isInfixOf` code =
            "HINT: In Haskell, you must surround string literals with double-quote '\"'. Such as \"Hello, world\"."
      | "Variable not in scope: main :: IO" `Text.isInfixOf` msg =
        "HINT: This error indicates you haven't defined main function."
      | "Variable not in scope:" `Text.isInfixOf` msg =
        "HINT: you might have misspelled 'putStrLn'."
      | otherwise = ""

    exercise2 =
      Exercise "2" $ runHaskellExercise diag2 "20.761245674740486\n"

    diag2 :: Diagnosis
    diag2 code msg
      | "parse error" `Text.isInfixOf` msg || "Parse error" `Text.isInfixOf` msg =
        if "top-level declaration expected." `Text.isInfixOf` msg
          then
            "HINT: This error indicates you haven't defined main function."
          else
            case compare (Text.count "(" code) (Text.count ")" code) of
                GT -> "HINT: you might have forgot to write close parenthesis"
                LT -> "HINT: you might have forgot to write open parenthesis"
                EQ -> ""
      | "No instance for (Fractional (IO ()))" `Text.isInfixOf` msg || "No instance for (Num (IO ()))" `Text.isInfixOf` msg =
        "HINT: you might have forgot to write parentheses"
      | "No instance for (Show (a0 -> a0))" `Text.isInfixOf` msg =
        "HINT: you might have forgot to write some numbers between operators ('*', '/' etc.)."
      | "No instance for (Num (t0 -> a0))" `Text.isInfixOf` msg =
        "HINT: you might have forgot to write multiplication operator '*'"
      | "No instance for (Fractional (t0 -> a0))" `Text.isInfixOf` msg =
        "HINT: you might have forgot to write division operator '/'"
      | "Variable not in scope: main :: IO" `Text.isInfixOf` msg =
        "HINT: This error indicates you haven't defined main function."
      | otherwise = ""

    exercise3 =
      Exercise "3" $ runHaskellExercise diag3 $ Text.unlines $
        [ "#     # ####### #       #        #####"
        , "#     # #       #       #       #     #"
        , "#     # #       #       #       #     #"
        , "####### #####   #       #       #     #"
        , "#     # #       #       #       #     #"
        , "#     # #       #       #       #     #"
        , "#     # ####### ####### #######  #####"
        ]

    diag3 :: Diagnosis
    diag3 code msg
      | "parse error on input" `Text.isInfixOf` msg
          && "'" `Text.isInfixOf` code =
            "HINT: In Haskell, you must surround string literals with double-quote '\"'. Such as \"Hello, world\"."
      | "Variable not in scope: main :: IO" `Text.isInfixOf` msg =
        "HINT: This error indicates you haven't defined main function."
      | "Variable not in scope:" `Text.isInfixOf` msg =
        "HINT: you might have misspelled 'putStrLn'."
      | "Couldn't match expected type ‘(String -> IO ())" `Text.isInfixOf` msg =
        if "do" `elem` Text.words code
          then "HINT: instructions in a `do` must be in a consistent width. "
          else "HINT: You seem to forget to write `do`. `do` must be put before listing `putStrLn`s."
      | otherwise = ""


runHaskellExercise :: Diagnosis -> Text -> Env -> FilePath -> IO Result
runHaskellExercise diag right e prgFile = do
  result <- runHaskell e prgFile
  case result of
      Right (outB, _errB {- TODO: print stderr -}) -> do
        let out = canonicalizeNewlines outB
            msg =
              Text.unlines
                [ Text.replicate 80 "="
                , "Your program's output: " <> Text.pack (show out) -- TODO: pretty print
                , "      Expected output: " <> Text.pack (show right)
                ]
        return $
          if out == right
            then Success $ "Nice output!\n\n" <> msg
            else Fail $ "Wrong output!\n\n" <> msg
      Left err -> do
        traceM $ "err: " ++ show err
        case err of
            RunHaskell.RunHaskellNotFound ->
              return $ Error "runhaskell command is not available.\nInstall stack or Haskell Platform."
            RunHaskell.RunHaskellFailure _ msg -> do
              logDebug e $ "RunHaskellFailure: " <> msg
              code <- Text.readFile prgFile
              return $ Fail $ appendDiagnosis diag code msg


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
  Paths_makeMistakesToLearnHaskell.getDataFileName ("assets/" ++ exerciseName ex ++ ext)
    >>= Text.readFile


loadDescriptionById :: ExerciseId -> IO (Maybe Text)
loadDescriptionById n = MaybeT.runMaybeT $ do
  ex <- Error.hoistMaybe $ getById n
  liftIO $ loadDescription ex


-- Handle error internally.
-- Because lastShownId is usually saved internally.
loadLastShown :: Env -> IO Exercise
loadLastShown e =
  loadLastShownId e >>=
    dieWhenNothing "Assertion failure: Invalid lastShownId saved! " . getById


getById :: ExerciseId -> Maybe Exercise
getById n = exercises !? (n - 1)


unsafeGetById :: ExerciseId -> Exercise
unsafeGetById n = exercises ! (n - 1)
