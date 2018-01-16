module Education.MakeThemToLearnHaskellSpec (main, spec) where

import           Test.Hspec

import qualified Paths_makeThemToLearnHaskell

import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.ByteString.Char8 as ByteString'
import           Data.Function ((&))
import qualified System.Directory as Dir
import qualified System.Environment as Env
import           System.Exit (ExitCode(ExitSuccess, ExitFailure))
import           System.FilePath ((</>))
import           System.Process.Typed (readProcess, readProcess_)
import qualified System.Process.Typed as Process


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "mtlh verify" $
    describe "when the user has never shown any exercise" $ do
      it "given the correct answer of exercise 1, show SUCCESS" $ do
        answerFile <- Paths_makeThemToLearnHaskell.getDataFileName ("assets" </> "1.hs")
        (code, out, err) <- runMtlh ["verify", answerFile]
        code `shouldBe` ExitSuccess
        err `shouldSatisfy` ByteString.null
        out `shouldSatisfy` includes "SUCCESS"

      it "given an empty answer, show FAIL" $
        runMtlh ["verify", "test/assets/common/empty.hs"]
          >>= shouldExitWithHints ["HINT: This error indicates you haven't defined main function."]

      it "given an answer whose 'Hello, world!' is singlequoted, show FAIL" $
        runMtlh ["verify", "test/assets/1/single-quote.hs"]
          >>= shouldExitWithHints ["HINT: In Haskell, you must surround string literals with double-quote '\"'. Such as \"Hello, world\"."]

      it "given an answer whose 'Hello, world!' is singlequoted and putStrLn isn't assigned to main, show FAIL" $
        runMtlh ["verify", "test/assets/1/single-quote-no-main.hs"]
          >>= shouldExitWithHints
            [ "HINT: This error indicates you haven't defined main function."
            , "HINT: In Haskell, you must surround string literals with double-quote '\"'. Such as \"Hello, world\"."
            ]

      it "given an answer with typo, show FAIL" $
        runMtlh ["verify", "test/assets/1/typo.hs"]
          >>= shouldExitWithHints ["HINT: you might have misspelled 'putStrLn'."]


runMtlh :: [String] -> IO (ExitCode, ByteString, ByteString)
runMtlh args = do
  (result, _) <- readProcess_ "stack path --local-install-root"
  let binPath = ByteString.unpack $ ByteString.takeWhile (not . isNewLine) result
  tmpDir <- (</> "tmp/mtlh") <$> Dir.getCurrentDirectory
  Env.setEnv "MAKE_THEM_TO_LEARN_HASKELL_HOME" tmpDir
  let cfg =
        Process.proc (binPath </> "bin" </> "mtlh") args
          & Process.setStdin Process.closed
  readProcess cfg


isNewLine :: Char -> Bool
isNewLine c = c == '\n' || c == '\r'


includes :: ByteString -> ByteString -> Bool
includes s =
  ByteString'.isInfixOf (ByteString.toStrict s) . ByteString.toStrict


shouldExitWithHints :: [ByteString] -> (ExitCode, ByteString, ByteString) -> IO ()
shouldExitWithHints hintMsgs (code, out, err) = do
  err `shouldSatisfy` ByteString.null
  mapM_ ((out `shouldSatisfy`) . includes) hintMsgs
  code `shouldBe` ExitFailure 1
