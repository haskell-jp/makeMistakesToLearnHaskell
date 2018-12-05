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

import qualified Options.Applicative as Opt
import           System.Console.ANSI

main :: IO ()
main = do
  avoidCodingError
  args <- Env.getArgs
  if null args then
    printExerciseList
  else do
    cmd <- Opt.execParser (Opt.info (cmdParser <**> Opt.helper) Opt.idm)
    withMainEnv $ \e ->
      case cmd of
        Show isTerminal n -> showExercise e isTerminal [n]
        Verify path -> verifySource e [path]


withMainEnv :: (Env -> IO r) -> IO r
withMainEnv doAction = do
  d <- Env.getEnv homePathEnvVarName <|> Dir.getXdgDirectory Dir.XdgData appName
  Dir.createDirectoryIfMissing True d
  IO.withFile (d </> "debug.log") IO.WriteMode $ \h -> do
    let e = defaultEnv
              { logDebug = ByteString.hPutStr h . (<> "\n")
              , appHomePath = d
              , runHaskell = RunHaskell.runFile e
              }
    doAction e

data Cmd
  = Show Bool String
  | Verify FilePath
  deriving (Eq, Show)

optTerminalP :: Opt.Parser Bool
optTerminalP = Opt.switch $ Opt.long "terminal" <> Opt.help "display to terminal"

showCmdP :: Opt.Parser Cmd
showCmdP = Show <$> optTerminalP
                <*> Opt.argument Opt.str (Opt.metavar "<number>")

verifyCmdP :: Opt.Parser Cmd
verifyCmdP = Verify <$> Opt.argument Opt.str (Opt.metavar "<filepath>")

cmdParser :: Opt.Parser Cmd
cmdParser = Opt.hsubparser
  $  Opt.command "show" (Opt.info showCmdP (Opt.progDesc "Show Exercise"))
  <> Opt.command "verify" (Opt.info verifyCmdP (Opt.progDesc "Verify Exercise"))


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
        withSGR [SetColor Foreground Vivid Green] $
          putStrLn "\n\nSUCCESS: Congratulations! Your solution got compiled and ran correctly!"

        showExampleSolution currentExercise
        Exit.exitSuccess

      Exercise.Fail details -> do
        Text.putStrLn details
        withSGR [SetColor Foreground Vivid Red] $
          putStrLn "\nFAIL: Your solution didn't pass. Try again!"
        putStrLn $ "HINT: Verified the exercise " ++ Exercise.name currentExercise ++ ". Note I verify the last `mmlh show`-ed exercise.\n"
        Exit.exitFailure

      Exercise.Error details -> do
        Error.errLn $ Text.toStrict $ details <> "\n\n"
        die "An unexpected error occurred when evaluating your solution."

      Exercise.NotVerified -> do
        putStrLn $ "[NOT VERIFIED] the exercise " ++ Exercise.name currentExercise ++ " has no test. Go ahead!"
        Exit.exitSuccess

      Exercise.NotYetImplemented -> do
        putStrLn
          $ "[NOT YET IMPLEMENTED] Sorry, the test of exercise "
          ++ Exercise.name currentExercise
          ++ " is not yet implemented. Check by yourself!"

        showExampleSolution currentExercise
        Exit.exitSuccess
  where
    withSGR sgrs act = bracket_ (setSGR sgrs) (setSGR [Reset]) act

    showExampleSolution ex = do
        putStrLn $ "Here's an example solution of the exercise " ++ Exercise.name ex ++ ":\n"
        Text.putStr =<< Exercise.loadExampleSolution ex


showExercise :: Env -> Bool -> [String] -> IO ()
showExercise _ _ [] = die "Specify an exercise number to show"
showExercise env isTerminal (n : _) = do
  d <- Exercise.loadDescriptionByName n
        >>= dieWhenNothing ("Exercise id " ++ n ++ " not found!")
  Exercise.saveLastShownName env n
  showMarkdown d isTerminal n

showMarkdown :: Text -> Bool -> String -> IO ()
showMarkdown md isTerminal n = do
  cssPath <- ("file://" <>) . TextS.pack <$> Paths.getDataFileName "assets/exercise.css"
  let htmlBody = CMark.commonmarkToHtml [CMark.optSafe] $ Text.toStrict md
      htmlHead = TextS.unlines
        [ "<!DOCTYPE html>"
        , "<html>"
        , "<head>"
        , "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />"
        , "<link rel=\"stylesheet\" type=\"text/css\" href=\"" <> cssPath <> "\" />"
        , "</head>"
        , "<body>"
        , "<div id=\"container\">"
        ]
      htmlFoot = TextS.unlines
        [ "</div>"
        , "</body>"
        , "</html>"
        ]

      mkHtmlPath dir = dir <> "/" <> "mmlh-ex" <> n <> ".html"
  path <- mkHtmlPath <$> Dir.getTemporaryDirectory

  writeUtf8FileS path (htmlHead <> htmlBody <> htmlFoot)

  browserLaunched <-
    if not isTerminal then
      Browser.openBrowser path
    else
      return False

  unless browserLaunched $
    Text.putStr $ removeAllTrailingSpace md
