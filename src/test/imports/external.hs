import           Test.Hspec
import           Test.Hspec.Core.Spec             (SpecM)
import           Test.Main                        (ProcessResult (ProcessResult, prException, prExitCode, prStderr, prStdout),
                                                   captureProcessResult,
                                                   withArgs, withEnv, withStdin)

import qualified Paths_makeMistakesToLearnHaskell as Paths

import           Control.Concurrent               (forkIO)
import           Control.Exception                (onException, try)
import           Control.Monad                    (void)
import           Data.ByteString.Char8            (ByteString)
import qualified Data.ByteString.Char8            as ByteString'
import qualified Data.ByteString.Char8            as ByteString
import           Data.Foldable                    (for_)
import           Data.Function                    ((&))
import qualified Data.Text                        as Text
import           GHC.Stack                        (HasCallStack)
import qualified System.Directory                 as Dir
import qualified System.Environment               as Env
import           System.Exit                      (ExitCode (ExitFailure, ExitSuccess))
import           System.FilePath                  ((</>))
