{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell
  ( main
  ) where


#include <imports/external.hs>

import           Education.MakeMistakesToLearnHaskell.Env
import qualified Education.MakeMistakesToLearnHaskell.Exercise as Exercise
import qualified Education.MakeMistakesToLearnHaskell.Evaluator.RunHaskell as RunHaskell
import           Education.MakeMistakesToLearnHaskell.Error


main :: IO ()
main = do
  avoidCodingError
  args <- Env.getArgs
  withMainEnv $ \e ->
    case args of
        ("verify" : left) -> verifySource e left
        ("show" : left) -> showExercise e left
        _ -> printExerciseList


withMainEnv :: (Env -> IO r) -> IO r
withMainEnv action = do
  d <- Env.getEnv homePathEnvVarName <|> Dir.getXdgDirectory Dir.XdgData appName
  Dir.createDirectoryIfMissing True d
  loc <- maybe Browser read <$> Env.lookupEnv terminalOutputEnvVarName
  IO.withFile (d </> "debug.log") IO.WriteMode $ \h -> do
    let e = defaultEnv
              { logDebug = ByteString.hPutStr h . (<> "\n")
              , appHomePath = d
              , runHaskell = RunHaskell.runFile e
              , envVerifyOutputLocation = loc
              }
    action e


printExerciseList :: IO ()
printExerciseList = do
  Text.putStrLn "# Make Mistakes to Learn Haskell!"
  Text.putStrLn ""
  Text.putStrLn "## Contents"

  let printHeader h = Text.putStrLn $ "- " <> h
  mapM_ printHeader =<< Exercise.loadHeaders

  Text.putStrLn $ "\nRun `" <> Text.pack appName <> " show <the exercise number>` to try the exercise."


verifySource :: Env -> [FilePath] -> IO ()
verifySource _ [] = die "Specify the Haskell source file to veirfy!"
verifySource e (file : _) = do
  currentExercise <- Exercise.loadLastShown e
  result <- Exercise.verify currentExercise e file
  case result of
      Exercise.Success details -> do
        Text.putStrLn details
        putStrLn "\n\nSUCCESS: Congratulations! Your solution got compiled and ran correctly!"
        putStrLn "Here's an example solution of this exercise:\n"
        Text.putStr =<< Exercise.loadExampleSolution currentExercise
        Exit.exitSuccess
      Exercise.Fail details -> do
        Text.putStrLn $ details <> "\n\nFAIL: Your solution didn't pass. Try again!"
        Exit.exitFailure
      Exercise.Error details -> do
        Error.errLn $ Text.toStrict $ details <> "\n\n"
        die "An unexpected error occurred when evaluating your solution."
      Exercise.NotVerified -> do
        Text.putStrLn "[NOT VERIFIED] This exercise has no test. Go ahead!"
        Exit.exitSuccess
      Exercise.NotYetImplemented -> do
        Text.putStrLn "[NOT YET IMPLEMENTED] Sorry, this exercise's test is not yet implemented. Check by yourself!"
        putStrLn "Here's an example solution of this exercise:\n"
        Text.putStr =<< Exercise.loadExampleSolution currentExercise
        Exit.exitSuccess


showExercise :: Env -> [String] -> IO ()
showExercise _ [] = die "Specify an exercise number to show"
showExercise env (n : _) = do
  d <- Exercise.loadDescriptionByName n
        >>= dieWhenNothing ("Exercise id " ++ n ++ " not found!")
  Exercise.saveLastShownName env n
  showMarkdown env d n

showMarkdown :: Env -> Text -> String -> IO ()
showMarkdown env md n = do
  let htmlContent = CMark.commonmarkToHtml [CMark.optSafe] $ Text.toStrict md
      mkHtmlPath dir = dir <> "/" <> "mmlh-ex" <> n <> ".html"
  path <- mkHtmlPath <$> Dir.getTemporaryDirectory

  TextS.writeFile path htmlContent

  isSuccess <-
    if isBrowser (envVerifyOutputLocation env) then
      Browser.openBrowser path
    else do
      Text.putStr md
      return True

  if isSuccess then
    return ()
  else
    -- ブラウザの起動に失敗した場合はコンソールに出力する
    Text.putStr md