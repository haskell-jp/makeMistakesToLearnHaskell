{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskellSpec (main, spec) where

#include <test/imports/external.hs>

import qualified Education.MakeMistakesToLearnHaskell
import           Education.MakeMistakesToLearnHaskell.Env


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


spec :: Spec
spec =
  describe "mmlh verify" $ do
    it "given the correct answer of exercise 1, show SUCCESS" $ do
      void $ runMmlh ["show", "--terminal", "1"]
      answerFile <- Paths.getDataFileName ("assets" </> "1.hs")
      runMmlh ["verify", answerFile] >>= shouldVerifySuccess

    it "given an empty answer, show FAIL" $ do
      void $ runMmlh ["show", "--terminal", "1"]
      runMmlh ["verify", "test/assets/common/empty.hs"]
        >>= shouldExitWithHints ["HINT: This error indicates that you haven't defined the main function."]

    it "given non-existing answer of exercise 2.5, show NOT VERIFIED" $ do
      void $ runMmlh ["show", "--terminal", "2.5"]
      runMmlh ["verify", "non-existing"] >>= shouldPrintNotVerified

    it "given the correct answer of exercise 4, show SUCCESS" $ do
      answerFile <- Paths.getDataFileName ("assets" </> "4.hs")
      void $ runMmlh ["show", "--terminal", "4"]
      runMmlh ["verify", answerFile] >>= shouldVerifySuccess

    it "given a wrong answer of exercise 4, show FAIL" $ do
      let msgs = ["Your program's output:", "Expected output:"]
      void $ runMmlh ["show", "--terminal", "4"]
      runMmlh ["verify", "test/assets/4/wrong-output.hs"]
        >>= shouldExitWithHints msgs

    it "given a not-compilable answer of exercise 4, show FAIL" $ do
      let msgs =
            ["HINT: You seem to have forgotten to write `do`. `do` must be put before listing `putStr`s and `getContents`."]
      void $ runMmlh ["show", "--terminal", "4"]
      runMmlh ["verify", "test/assets/4/no-do.hs"]
        >>= shouldExitWithHints msgs


runMmlh :: [String] -> IO ProcessResult
runMmlh args = do
  Dir.createDirectoryIfMissing False "./tmp/"
  let env = [ (homePathEnvVarName, Just "./tmp/")]
  withArgs args
    $ withEnv env
    $ captureProcessResult Education.MakeMistakesToLearnHaskell.main


includes :: ByteString'.ByteString -> ByteString'.ByteString -> Bool
includes = ByteString'.isInfixOf


shouldExitWithHints :: [ByteString'.ByteString] -> ProcessResult -> IO ()
shouldExitWithHints hintMsgs (ProcessResult out err code ex) = do
  fmap show ex `shouldBe` Nothing
  err `shouldSatisfy` ByteString'.null
  mapM_ ((out `shouldSatisfy`) . includes) hintMsgs
  code `shouldBe` ExitFailure 1


shouldVerifySuccess :: ProcessResult -> IO ()
shouldVerifySuccess (ProcessResult out err code ex) = do
  fmap show ex `shouldBe` Nothing
  err `shouldSatisfy` ByteString'.null
  out `shouldSatisfy` includes "SUCCESS"
  code `shouldBe` ExitSuccess


shouldPrintNotVerified :: ProcessResult -> IO ()
shouldPrintNotVerified (ProcessResult out err code ex) = do
  fmap show ex `shouldBe` Nothing
  err `shouldSatisfy` ByteString'.null
  out `shouldSatisfy` includes "NOT VERIFIED"
  code `shouldBe` ExitSuccess
