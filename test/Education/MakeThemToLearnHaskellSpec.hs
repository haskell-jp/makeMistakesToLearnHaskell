{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeThemToLearnHaskellSpec (main, spec) where

#include <test/imports/external.hs>


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


newtype TestEnv = TestEnv { mtlhPath :: FilePath }


prepareTestEnv :: SpecM a TestEnv
prepareTestEnv = runIO $ do
  tmpDir <- (</> "tmp/mtlh") <$> Dir.getCurrentDirectory
  Env.setEnv "MAKE_THEM_TO_LEARN_HASKELL_HOME" tmpDir

  (result, _) <- readProcess_ "stack path --local-install-root"
  let binPath =
        ByteString.unpack $ ByteString.takeWhile (not . isNewLine) result

  return $ TestEnv $ binPath </> "bin" </> "mtlh"

spec :: Spec
spec = do
  e <- prepareTestEnv
  describe "mtlh verify" $
    describe "when the user has never shown any exercise" $ do
      it "given the correct answer of exercise 1, show SUCCESS" $ do
        answerFile <- Paths_makeThemToLearnHaskell.getDataFileName ("assets" </> "1.hs")
        (code, out, err) <- runMtlh e ["verify", answerFile]
        err `shouldSatisfy` ByteString.null
        out `shouldSatisfy` includes "SUCCESS"
        code `shouldBe` ExitSuccess

      it "given an empty answer, show FAIL" $
        runMtlh e ["verify", "test/assets/common/empty.hs"]
          >>= shouldExitWithHints ["HINT: This error indicates you haven't defined main function."]


runMtlh :: TestEnv -> [String] -> IO (ExitCode, ByteString, ByteString)
runMtlh e args =
  readProcess $
    Process.proc (mtlhPath e) args
      & Process.setStdin Process.closed


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
