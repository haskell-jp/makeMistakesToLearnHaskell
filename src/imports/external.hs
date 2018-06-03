-- Common import statements for external libraries.
-- Intended to include by CPP.

import           Control.Applicative ((<|>))
import qualified Control.Error as Error
import           Control.Exception
                   ( Exception
                   , SomeException
                   , bracket_
                   , catch
                   , throwIO
                   , throw
                   )
import           Control.Monad (zipWithM_, void)
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.Maybe as MaybeT
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import           Data.Maybe (fromMaybe, maybeToList)
import           Data.Monoid ((<>))
import qualified Data.Text.Encoding.Error as TextEncoding
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import qualified Data.Text.Lazy.Encoding as TextEncoding
import           Data.Typeable (Typeable)
import           Data.Vector (Vector, (!?), (!))
import qualified Data.Vector as Vector
import qualified Data.Yaml as Yaml
import qualified Data.Yaml.TH as Yaml
import qualified Debug.Trace as Debug
import           GHC.Generics (Generic)
import           Safe (readMay, headMay)
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit as Exit
import           System.Exit (ExitCode(ExitSuccess, ExitFailure))
import           System.FilePath ((</>))
import qualified System.IO as IO
import           System.Process.Typed (readProcess)
import qualified System.Process.Typed as Process
#ifdef mingw32_HOST_OS
import qualified System.Win32.Console as Win32
import GHC.IO.Encoding.CodePage (mkLocaleEncoding)
import GHC.IO.Encoding.Failure (CodingFailureMode(TransliterateCodingFailure))
#endif
