{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell
  ( main
  ) where


#include <imports/external.hs>

import           Education.MakeMistakesToLearnHaskell.Env
import qualified Education.MakeMistakesToLearnHaskell.Exercise as Exercise
import qualified Education.MakeMistakesToLearnHaskell.Evaluator.RunHaskell as RunHaskell
import           Education.MakeMistakesToLearnHaskell.Error
import           Education.MakeMistakesToLearnHaskell.Text

import Options.Applicative

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
  loc <- maybe Browser read <$> Env.lookupEnv showExerciseOutputEnvVarName
  IO.withFile (d </> "debug.log") IO.WriteMode $ \h -> do
    let e = defaultEnv
              { logDebug = ByteString.hPutStr h . (<> "\n")
              , appHomePath = d
              , runHaskell = RunHaskell.runFile e
              , envShowExerciseOutputLocation = loc
              }
    action e

data Cmd
  = Show Bool Int
  | Verify FilePath
  deriving (Eq, Show)

optOpenBrowserP :: Parser Bool
optOpenBrowserP = switch $ long "open" <> help "show exercise in browser"

showCmdP :: Parser Cmd
showCmdP = Show <$> optOpenBrowserP
                <*> argument auto (metavar "<number>")

verifyCmdP :: Parser Cmd
verifyCmdP = Verify <$> argument str (metavar "<filepath>")

cmdParser :: Parser Cmd
cmdParser = subparser
  $  command "show" (info showCmdP (progDesc "Show Exercise"))
  <> command "verify" (info verifyCmdP (progDesc "Verify Exercise"))


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
  putStrLn "==================== GHC output ===================="
  case result of
      Exercise.Success details -> do
        Text.putStrLn details
        putStrLn "\n\nSUCCESS: Congratulations! Your solution got compiled and ran correctly!"
        putStrLn "Here's an example solution of this exercise:\n"
        Text.putStr =<< Exercise.loadExampleSolution currentExercise
        Exit.exitSuccess
      Exercise.Fail details -> do
        mapM_ Text.putStrLn
          [ details
          , ""
          , "FAIL: Your solution didn't pass. Try again!"
          , ""
          ]
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

  writeUtf8FileS path htmlContent

  browserLaunched <-
    if envShowExerciseOutputLocation env == Browser then
      Browser.openBrowser path
    else
      return False

  unless browserLaunched $
    Text.putStr $ removeAllTrailingSpace md
