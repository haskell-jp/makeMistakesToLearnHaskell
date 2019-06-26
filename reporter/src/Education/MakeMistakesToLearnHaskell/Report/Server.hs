{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Education.MakeMistakesToLearnHaskell.Report.Server
    ( Report (..)
    , Result (..)
    , API
    , startApp
    , app
    , api

    , startStub
    ) where

import           Control.Exception                                     (SomeException,
                                                                        handle,
                                                                        throwIO)
import           Control.Monad                                         (unless)
import           Control.Monad.IO.Class                                (MonadIO,
                                                                        liftIO)
import qualified Data.Aeson.TH                                         as AT
import qualified Data.ByteString.Lazy                                  as B
import qualified Data.Text.Lazy                                        as T
import qualified Data.Text.Lazy.Encoding                               as TE
import qualified Data.Text.Lazy.IO                                     as TI
import qualified Data.ULID                                             as U
import           GHC.Generics                                          (Generic)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.Directory                                      (doesDirectoryExist,
                                                                        doesFileExist,
                                                                        withCurrentDirectory)
import           System.Environment                                    (getEnv)
import qualified System.FileLock                                       as FL
import qualified System.Process.Typed                                  as P

import qualified Education.MakeMistakesToLearnHaskell.Commons.Exercise as Exercise


data Report = Report
  { exerciseName   :: Exercise.Name
  , exerciseAnswer :: Exercise.SourceCode
  , exerciseFailBy :: Exercise.FailBy
  } deriving (Eq, Show, Generic)

$(AT.deriveJSON AT.defaultOptions ''Report)


newtype Result = Result { reportUrl :: T.Text } deriving (Eq, Show)

$(AT.deriveJSON AT.defaultOptions ''Result)


type API =
  Get '[PlainText] T.Text
    :<|> "reports" :> ReqBody '[JSON] Report :> Post '[JSON] Result


type GithubAccessToken = String


startApp :: IO ()
startApp = do
  at <- getEnv "GITHUB_ACCESS_TOKEN"
  p <- read <$> getEnv "PORT" -- Apps running on Heroku must read the port number by this envvar.

  runGit_ ["config", "--global", "user.email", "whosekiteneverfly+haskelljp@gmail.com"]
  runGit_ ["config", "--global", "user.name", "Haskell-jp Bot"]

  e <- doesDirectoryExist $ repositoryName ++ "/.git"
  unless e $
    runGit_ ["clone", "--depth", "1", repositoryUrl at]

  run p app


startStub :: Int -> IO ()
startStub port = run port $ serve api stubServer


app :: Application
app = serve api server


api :: Proxy API
api = Proxy


server :: Server API
server = pong :<|> postReport


stubServer :: Server API
stubServer = stubPong :<|> stubPostReport


stubPong :: Handler T.Text
stubPong = return "STUB SERVER: It works!\n"


stubPostReport :: Report -> Handler Result
stubPostReport r = return . Result . T.pack $ show r


pong :: Handler T.Text
pong = return "It works!\n"


postReport :: Report -> Handler Result
postReport r =
  liftIO . withCurrentDirectory repositoryName . locking $ do
    bname <- show <$> U.getULID
    runGit_ ["checkout", "-b", bname]

    createReportCommitToGithub r

    runGit_ ["push", "--set-upstream", "origin", bname]
    runGit_ ["push", "-u"]
    resultFromSha <$> getHeadSha


createReportCommitToGithub :: Report -> IO ()
createReportCommitToGithub r = liftIO $ do
  let answerFile = "answer.hs"
  TI.writeFile answerFile $ exerciseAnswer r
  otherFiles <- writeFailBy $ exerciseFailBy r
  runGit_ $ "add" : answerFile : otherFiles

  runGit_ ["commit", "-m", "Exercise " ++ exerciseName r]


writeFailBy :: Exercise.FailBy -> IO [FilePath]
writeFailBy (Exercise.WrongOutput d) = do
  let file = "wrong-output.details.txt"
  TI.writeFile file d
  return [file]
writeFailBy (Exercise.CommandFailed cmd dCmd dDiag) = do
  let outputFile = cmd ++ ".output.txt"
      diagFile = "diagnosis.txt"
  TI.writeFile outputFile dCmd
  TI.writeFile diagFile dDiag
  return [outputFile, diagFile]


getHeadSha :: MonadIO m => m T.Text
getHeadSha = do
  (out, err) <- P.readProcess_ $ P.proc "git" ["rev-parse", "HEAD"]
  liftIO $ B.putStr err
  return $ TE.decodeUtf8 out


locking :: IO a -> IO a
locking act = handle (\e -> print (e :: SomeException) >> throwIO e) $ do
  let lockFile = ".mmlh-report-server.lock"
  e <- doesFileExist lockFile
  unless e $ writeFile lockFile ""
  FL.withFileLock lockFile FL.Exclusive (const act)


resultFromSha :: T.Text -> Result
resultFromSha = Result . ("https://github.com/haskell-jp/makeMistakesToLearnHaskell-support/commit/" <>)


runGit_ :: MonadIO m => [String] -> m ()
runGit_ = P.runProcess_ . P.proc "git"


repositoryName :: FilePath
repositoryName = "makeMistakesToLearnHaskell-support"


repositoryUrl :: GithubAccessToken -> String
repositoryUrl at =
  "https://haskell-jp-bot:" ++ at ++ "@github.com/haskell-jp/" ++ repositoryName ++ ".git"
