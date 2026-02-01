{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskellSpec (main, spec) where

#include <test/imports/external.hs>

import           Education.MakeMistakesToLearnHaskell.Report.Server (startStub)

import qualified Education.MakeMistakesToLearnHaskell
import           Education.MakeMistakesToLearnHaskell.Env


testServerPort :: Int
testServerPort = 9125


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  _ <- runIO . forkIO $ startStub testServerPort
  describe "mmlh verify" $ do
    it "given the correct answer of exercise 1, show SUCCESS" $ do
      void $ runMmlh ["show", "--terminal", "1"] ""
      answerFile <- Paths.getDataFileName ("assets" </> "1.hs")
      runMmlh ["verify", answerFile] "" >>= shouldVerifySuccess

    it "given an empty answer, show FAIL" $ do
      void $ runMmlh ["show", "--terminal", "1"] ""
      runMmlh ["verify", "test/assets/common/empty.hs"] ""
        >>= shouldExitWithMessages ["HINT: This error indicates you haven't defined main function."]

    it "given non-existing answer of exercise 2.5, show NOT VERIFIED" $ do
      void $ runMmlh ["show", "--terminal", "2.5"] ""
      runMmlh ["verify", "non-existing"] "" >>= shouldPrintNotVerified

    itShouldShowSuccessGivenExampleAnswerOf "4"
    itShouldShowSuccessGivenExampleAnswerOf "6"
    itShouldShowSuccessGivenExampleAnswerOf "12"
    itShouldShowSuccessGivenExampleAnswerOf "13"
    itShouldShowSuccessGivenExampleAnswerOf "14"
    itShouldShowSuccessGivenExampleAnswerOf "15"

    it "given a not-compilable answer of exercise 4, show FAIL" $ do
      let msgs =
            ["HINT: You seem to forget to write `do`. `do` must be put before listing `putStr`s and `getContents`."]
      void $ runMmlh ["show", "--terminal", "4"] ""
      runMmlh ["verify", "test/assets/4/no-do.hs"] ""
        >>= shouldExitWithMessages msgs

    it "given the wrong answer (producing a wrong result given a correct input) of exercise 12, show SUCCESS" $ do
      let msgs = ["Your program's output:", "Expected output:"]
      void $ runMmlh ["show", "--terminal", "12"] ""
      runMmlh ["verify", "test/assets/12/wrong-output1.hs"] ""
        >>= shouldExitWithMessages msgs

    it "given the wrong answer (producing a wrong result given a wrong input) of exercise 12, show SUCCESS" $ do
      let msgs = ["Your program's output:", "Expected output:"]
      void $ runMmlh ["show", "--terminal", "12"] ""
      runMmlh ["verify", "test/assets/12/wrong-output2.hs"] ""
        >>= shouldExitWithMessages msgs


    -- Response from the report server. The stub server actually desen't return a URL. But the production server is expected to.
    let expectedMessage n =
          "Open Report {exerciseName = \"" <> n <> "\", exerciseAnswer = \"\", exerciseFailBy = CompileError \""

    context "given \"y\" from stdin" $ do
      it "given a not-compilable answer of exercise 5, show FAIL with the URL to submit an issue to haskell-jp/" $ do
        void $ runMmlh ["show", "--terminal", "5"] ""
        runMmlh ["verify", "--terminal", "test/assets/common/empty.hs"] "y\n"
          >>= shouldExitWithMessages [expectedMessage "5"]

      it "given a not-compilable answer of exercise 6, show FAIL with the URL to submit an issue to haskell-jp/" $ do
        void $ runMmlh ["show", "--terminal", "6"] ""
        runMmlh ["verify", "--terminal", "test/assets/common/empty.hs"] "y\n"
          >>= shouldExitWithMessages [expectedMessage "6"]

    context "given nothing from stdin" $ do
      it "given a not-compilable answer of exercise 5, show FAIL without any URL to GitHub." $ do
        void $ runMmlh ["show", "--terminal", "5"] ""
        runMmlh ["verify", "--terminal", "test/assets/common/empty.hs"] ""
          >>= shouldNotExitWithMessages ["Open Report"]

      it "given a not-compilable answer of exercise 6, show FAIL without any URL to GitHub." $ do
        void $ runMmlh ["show", "--terminal", "6"] ""
        runMmlh ["verify", "--terminal", "test/assets/common/empty.hs"] ""
          >>= shouldNotExitWithMessages ["Open Report"]


itShouldShowSuccessGivenExampleAnswerOf :: String -> Spec
itShouldShowSuccessGivenExampleAnswerOf exerciseName =
  it ("given the correct answer of exercise " ++ exerciseName ++ ", show SUCCESS") $ do
    answerFile <- Paths.getDataFileName ("assets" </> exerciseName ++ ".hs")
    void $ runMmlh ["show", "--terminal", exerciseName] ""
    runMmlh ["verify", answerFile] "" >>= shouldVerifySuccess


runMmlh :: [String] -> ByteString'.ByteString -> IO ProcessResult
runMmlh args stdinDat = do
  Dir.createDirectoryIfMissing False "./tmp/"
  let env = [(homePathEnvVarName, Just "./tmp/")]
  withArgs args
    . withStdin stdinDat
    . withEnv env
    . captureProcessResult
    . Education.MakeMistakesToLearnHaskell.mainFromReportServer
    $ "http://localhost:" ++ show testServerPort

shouldContainBS :: ByteString'.ByteString -> ByteString'.ByteString -> Expectation
shouldContainBS a b = if ByteString'.isInfixOf b a
  then pure ()
  else expectationFailure $ unwords [show a, "does not contain", show b]

shouldExitWithMessagesLike
  :: HasCallStack
  => (ByteString'.ByteString -> ByteString'.ByteString -> Bool)
  -> [ByteString'.ByteString]
  -> ProcessResult
  -> IO ()
shouldExitWithMessagesLike p hintMsgs (ProcessResult out err code ex) = do
  fmap show ex `shouldBe` Nothing
  err `shouldSatisfy` ByteString'.null
  mapM_ ((out `shouldSatisfy`) . p) hintMsgs
  code `shouldBe` ExitFailure 1


shouldExitWithMessages :: HasCallStack => [ByteString'.ByteString] -> ProcessResult -> IO ()
shouldExitWithMessages = shouldExitWithMessagesLike (flip ByteString'.isInfixOf)


shouldNotExitWithMessages :: HasCallStack => [ByteString'.ByteString] -> ProcessResult -> IO ()
shouldNotExitWithMessages = shouldExitWithMessagesLike (\s -> not . (flip ByteString'.isInfixOf) s)


shouldVerifySuccess :: ProcessResult -> IO ()
shouldVerifySuccess (ProcessResult out err code ex) = (`onException` handle) $ do
  fmap show ex `shouldBe` Nothing
  err `shouldBe` ByteString'.empty
  out `shouldContainBS` "SUCCESS"
  code `shouldBe` ExitSuccess
 where
  handle = do
    putStrLn "\n=== STDOUT:"
    ByteString'.putStr out
    putStrLn "\n=== STDERR:"
    ByteString'.putStr err


shouldPrintNotVerified :: ProcessResult -> IO ()
shouldPrintNotVerified (ProcessResult out err code ex) = do
  fmap show ex `shouldBe` Nothing
  err `shouldBe` ByteString'.empty
  out `shouldContainBS` "NOT VERIFIED"
  code `shouldBe` ExitSuccess
