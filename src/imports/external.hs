-- Common import statements for external libraries.
-- Intended to include by CPP.

import qualified CMark
import           Control.Applicative ((<|>), optional, (<**>))
import qualified Control.Error as Error
import           Control.Exception
                   ( Exception
                   , IOException
                   , bracket_
                   , handle
                   , throwIO
                   , throw
                   )
import           Control.Monad (void, unless, when)
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.Maybe as MaybeT
import           Data.Bool (bool)
import qualified Data.ByteString.Char8 as ByteString'
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Char as Char
import           Data.Functor (($>))
import qualified Data.List as List
import           Data.Maybe (fromMaybe, maybeToList, isJust)
import           Data.IORef
                   ( newIORef
                   , readIORef
                   , writeIORef
                   )
import           Data.Monoid ((<>))
import qualified Data.Text.Encoding.Error as TextEncoding
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as TextEncoding
import qualified Data.Text as TextS
import           Data.Traversable (for)
import           Data.Typeable (Typeable)
import qualified Debug.Trace as Debug
import           GHC.Generics (Generic)
import qualified GHC.SyntaxHighlighter as GHC
import           Numeric.Natural (Natural)
import qualified Network.URI.Encode as Uri
import qualified Paths_makeMistakesToLearnHaskell as Paths
import           Safe (headMay)
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit as Exit
import           System.Exit (ExitCode(ExitSuccess, ExitFailure))
import           System.FilePath ((</>))
import qualified System.FilePath as FilePath
import qualified System.IO as IO
import qualified System.IO.Temp as Temp
import           System.Process.Typed (runProcess)
import qualified System.Process.Typed as Process
import qualified Test.QuickCheck as QuickCheck
import           Test.QuickCheck (Arbitrary, Gen, quickCheckWithResult, arbitrary)
import qualified Text.Regex.Applicative as Regex
import qualified Web.Browser as Browser
#ifdef mingw32_HOST_OS
import qualified System.Win32.Console as Win32
import GHC.IO.Encoding.CodePage (mkLocaleEncoding)
import GHC.IO.Encoding.Failure (CodingFailureMode(TransliterateCodingFailure))
#endif
