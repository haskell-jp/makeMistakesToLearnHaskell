{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskellSpec (main, spec) where

#include <test/imports/external.hs>

import qualified Education.MakeMistakesToLearnHaskell


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


spec :: Spec
spec =
  describe "mmlh verify" $
    describe "when the user has never shown any exercise" $ do
      it "given the correct answer of exercise 1, show SUCCESS" $ do
        answerFile <- Paths_makeMistakesToLearnHaskell.getDataFileName ("assets" </> "1.hs")
        ProcessResult out err code <- runMmlh ["verify", answerFile]
        err `shouldSatisfy` ByteString'.null
        out `shouldSatisfy` includes "SUCCESS"
        code `shouldBe` ExitSuccess

      it "given an empty answer, show FAIL" $
        runMmlh ["verify", "test/assets/common/empty.hs"]
          >>= shouldExitWithHints ["HINT: This error indicates you haven't defined main function."]


runMmlh :: [String] -> IO ProcessResult
runMmlh args =
  withArgs args $ captureProcessResult Education.MakeMistakesToLearnHaskell.main


includes :: ByteString'.ByteString -> ByteString'.ByteString -> Bool
includes = ByteString'.isInfixOf


shouldExitWithHints :: [ByteString'.ByteString] -> ProcessResult -> IO ()
shouldExitWithHints hintMsgs (ProcessResult out err code) = do
  err `shouldSatisfy` ByteString'.null
  mapM_ ((out `shouldSatisfy`) . includes) hintMsgs
  code `shouldBe` ExitFailure 1
