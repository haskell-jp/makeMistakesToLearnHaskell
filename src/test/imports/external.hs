import           Test.Hspec
import           Test.Hspec.Core.Spec (SpecM)

import qualified Paths_makeThemToLearnHaskell

import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.ByteString.Char8 as ByteString'
import           Data.Function ((&))
import qualified Data.Text.Lazy as Text
import qualified System.Directory as Dir
import qualified System.Environment as Env
import           System.Exit (ExitCode(ExitSuccess, ExitFailure))
import           System.FilePath ((</>))
import           System.Process.Typed (readProcess, readProcess_)
import qualified System.Process.Typed as Process
