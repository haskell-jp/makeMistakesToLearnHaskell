{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Education.MakeMistakesToLearnHaskell.Report.Server
    ( Report (..)
    , Result
    , reportUrl
    , API
    , startApp
    , app
    , api
    ) where

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
import           System.Directory                                      (createDirectory,
                                                                        doesDirectoryExist,
                                                                        doesFileExist,
                                                                        withCurrentDirectory,
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


type API = "reports" :> ReqBody '[JSON] Report :> Post '[JSON] Result


type GithubAccessToken = String


startApp :: IO ()
startApp = do
  a <- app <$> getEnv "GITHUB_ACCESS_TOKEN"
  p <- read <$> getEnv "PORT"
  run p a


app :: GithubAccessToken -> Application
app = serve api . server


api :: Proxy API
api = Proxy


server :: GithubAccessToken -> Server API
server = postReport


postReport :: GithubAccessToken -> Report -> Handler Result
postReport at r = liftIO $ do
  e <- doesDirectoryExist $ repositoryName ++ "/.git"
  unless e $
    runGit_ ["clone", "--depth", "1", "--branch", "reports", repositoryUrl at]

  withCurrentDirectory repositoryName . locking $ do
    createReportCommitToGithub r
    runGit_ ["push", "-u"]
    resultFromSha <$> getHeadSha


createReportCommitToGithub :: MonadIO m => Report -> m ()
createReportCommitToGithub r = liftIO $ do
  dname <- show <$> U.getULID
  createDirectory dname
  withCurrentDirectory dname $ do
    TI.writeFile "answer.hs" $ exerciseAnswer r
    writeFailBy $ exerciseFailBy r
    runGit_ ["add", "."]
    runGit_ ["commit", "-m", "Exercise " ++ exerciseName r]


writeFailBy :: Exercise.FailBy -> IO ()
writeFailBy (Exercise.WrongOutput d) =
  TI.writeFile "wrong-output.details.txt" d
writeFailBy (Exercise.CommandFailed cmd dCmd dDiag) = do
  TI.writeFile (cmd ++ ".output.txt") dCmd
  TI.writeFile "diagnosis.txt" dDiag


getHeadSha :: MonadIO m => m T.Text
getHeadSha = do
  (out, err) <- P.readProcess_ $ P.proc "git" ["rev-parse", "HEAD"]
  liftIO $ B.putStr err
  return $ TE.decodeUtf8 out


locking :: IO a -> IO a
locking act = do
  let lockFile = ".mmlh-report-server.lock"
  e <- doesFileExist lockFile
  unless e $ writeFile lockFile ""
  FL.withFileLock lockFile FL.Exclusive (const act)


resultFromSha :: T.Text -> Result
resultFromSha = Result . ("https://github.com/haskell-jp-bot/makeMistakesToLearnHaskell-support/commit/" <>)


runGit_ :: MonadIO m => [String] -> m ()
runGit_ = P.runProcess_ . P.proc "git"


repositoryName :: FilePath
repositoryName = "makeMistakesToLearnHaskell-support"


repositoryUrl :: GithubAccessToken -> String
repositoryUrl at =
  "https://haskell-jp-bot:" ++ at ++ "@github.com/haskell-jp-bot/" ++ repositoryName ++ ".git"
