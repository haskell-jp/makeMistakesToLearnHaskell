{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell
  ( productionMain
  , mainFromReportServer
  ) where


#include <imports/external.hs>
#include <imports/io.hs>

import           Education.MakeMistakesToLearnHaskell.Report.Client    (EndpointUrl)

import           Education.MakeMistakesToLearnHaskell.Env
import qualified Education.MakeMistakesToLearnHaskell.Exercise as Exercise
import qualified Education.MakeMistakesToLearnHaskell.Exercise.FormatMessage as Exercise
import qualified Education.MakeMistakesToLearnHaskell.Evaluator.Command as Command
import           Education.MakeMistakesToLearnHaskell.Error
import qualified Education.MakeMistakesToLearnHaskell.Report as Report
import           Education.MakeMistakesToLearnHaskell.Text

import qualified Options.Applicative as Opt
import           System.Console.ANSI

productionMain :: IO ()
productionMain = mainFromReportServer "https://mmlh-reporter.herokuapp.com/"

mainFromReportServer :: EndpointUrl -> IO ()
mainFromReportServer defaultHost = do
  avoidCodingError
  args <- Env.getArgs
  if null args then
    printExerciseList
  else do
    (copts, cmd) <- Opt.execParser (Opt.info (cmdParser <**> Opt.helper) Opt.idm)
    withMainEnv defaultHost copts $ \e ->
      case cmd of
        Show n ->
          showExercise e n
        Verify path ->
          verifySource e path


withMainEnv :: EndpointUrl -> CommonOptions -> (Env -> IO r) -> IO r
withMainEnv defaultHost copts doAction = do
  d <- Env.getEnv homePathEnvVarName <|> Dir.getXdgDirectory Dir.XdgData appName
  Dir.createDirectoryIfMissing True d

  host <- Env.getEnv reportServerEnvVarName <|> pure defaultHost

  let openB =
        if enableBrowser copts
          then Browser.openBrowser . Text.unpack
          else const $ return False

  IO.withFile (d </> "debug.log") IO.WriteMode $ \h -> do
    let e = defaultEnv
              { logDebug = ByteString.hPutStr h . (<> "\n")
              , appHomePath = d
              , executeCommand = Command.execute
              , confirm = \prompt -> do
                  Text.putStrLn $ prompt <> " (y/n)"
                  handle ((const $ return False) :: IOException -> IO Bool) $ do
                    ans <- getChar
                    return $ ans == 'y' || ans == 'Y'
              , openWithBrowser = openB
              , say = Text.putStrLn
              , postReport = IO.postReport host
              }
    doAction e

newtype CommonOptions = CommonOptions { enableBrowser :: Bool }


data Cmd
  = Show Exercise.Name
  | Verify FilePath
  deriving (Eq, Show)


commonOptionsP :: Opt.Parser CommonOptions
commonOptionsP =
  fmap (CommonOptions . not)
    . Opt.switch
    $ Opt.long "terminal" <> Opt.help "Display HTML/URL on terminal (Don't launch browser)."


showCmdP :: Opt.Parser (CommonOptions, Cmd)
showCmdP = (,)
  <$> commonOptionsP
  <*> (Show <$> Opt.argument Opt.str (Opt.metavar "<number>"))


verifyCmdP :: Opt.Parser (CommonOptions, Cmd)
verifyCmdP = (,)
  <$> commonOptionsP
  <*> (Verify <$> Opt.argument Opt.str (Opt.metavar "<filepath>"))


cmdParser :: Opt.Parser (CommonOptions, Cmd)
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


verifySource :: Env -> FilePath -> IO ()
verifySource e file = do
  currentExercise <- Exercise.loadLastShown e
  result <- Exercise.verify currentExercise e file
  case result of
      Exercise.Success details -> do
        Text.putStrLn details
        withSGR [SetColor Foreground Vivid Green] $
          putStrLn "\n\nSUCCESS: Congratulations! Your solution got compiled and ran correctly!"

        showExampleSolution currentExercise
        Exit.exitSuccess

      Exercise.Fail code details -> do
        Text.putStrLn $ Exercise.formatFailure details
        withSGR [SetColor Foreground Vivid Red] $
          putStrLn "\nFAIL: Your solution didn't pass. Try again!"
        putStrLn $ "HINT: Verified the exercise " ++ Exercise.name currentExercise ++ ". Note I verify the last `mmlh show`-ed exercise.\n"

        Report.printUrlIfAsked e (Exercise.name currentExercise) code details
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
          ++ " is not yet implemented.\n"
          ++ "So I confirmed only that the your answer is compilable.\n"
          ++ "Test by yourself!"

        showExampleSolution currentExercise
        Exit.exitSuccess
  where
    withSGR sgrs = bracket_ (setSGR sgrs) (setSGR [Reset])

    showExampleSolution ex = do
        putStrLn $ "Here's an example solution of the exercise " ++ Exercise.name ex ++ ":\n"
        Text.putStr =<< Exercise.loadExampleSolution ex


showExercise :: Env -> Exercise.Name -> IO ()
showExercise e n = do
  d <- Exercise.loadDescriptionByName n
        >>= dieWhenNothing ("Exercise id " ++ n ++ " not found!")
  Exercise.saveLastShownName e n
  showMarkdown e d n

showMarkdown :: Env -> Text -> String -> IO ()
showMarkdown e md n = do
  cssPath <- TextS.pack <$> Paths.getDataFileName "assets/exercise.css"
  let htmlBody = CMark.commonmarkToHtml [CMark.optSafe] $ Text.toStrict md
      htmlHead = TextS.unlines
        [ "<!DOCTYPE html>"
        , "<html>"
        , "<head>"
        , "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />"
        , "<link rel=\"stylesheet\" type=\"text/css\" href=\"file://" <> cssPath <> "\" />"
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

  browserLaunched <- openWithBrowser e (Text.pack path)

  unless browserLaunched $
    Text.putStr $ removeAllTrailingSpace md
