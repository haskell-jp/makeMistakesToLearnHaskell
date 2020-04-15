import           Test.Hspec
import           Test.Hspec.Core.Spec             (SpecM)
import           Test.Main                        (ProcessResult (ProcessResult, prException, prExitCode, prStderr, prStdout),
                                                   captureProcessResult,
                                                   withArgs, withEnv, withStdin)

import qualified Paths_makeMistakesToLearnHaskell as Paths

import           Control.Concurrent               (forkIO)
import           Control.Exception                (try, onException)
import           Control.Monad                    (void)
import qualified Data.ByteString.Char8            as ByteString'
import           Data.ByteString.Lazy.Char8       (ByteString)
import qualified Data.ByteString.Lazy.Char8       as ByteString
import           Data.Foldable                    (for_)
import           Data.Function                    ((&))
import qualified Data.Text.Lazy                   as Text
import qualified System.Directory                 as Dir
import qualified System.Environment               as Env
import           System.Exit                      (ExitCode (ExitFailure, ExitSuccess))
import           System.FilePath                  ((</>))
