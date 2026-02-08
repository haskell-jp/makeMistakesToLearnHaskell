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
import           Data.Bifunctor (bimap)
import           Data.Bool (bool)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy.Char8 as ByteStringLazy
import qualified Data.Char as Char
import           Data.Foldable (for_)
import           Data.Function (on)
import           Data.Functor (($>))
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, maybeToList, isJust)
import           Data.IORef (newIORef, readIORef, writeIORef)
import           Data.Monoid ((<>))
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Text.Encoding.Error as TextEncoding
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import           Data.Traversable (for)
import           Data.Typeable (Typeable)
import qualified Debug.Trace as Debug
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
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
import           System.Process.Typed (readProcessInterleaved)
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
