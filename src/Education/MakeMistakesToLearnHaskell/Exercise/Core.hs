{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Core
  ( runHaskellExerciseEq
  , runHaskellExerciseWithStdinEq
  , noVeirificationExercise
  , notYetImplementedVeirificationExercise
  , isInWords
  , detailsForgetToWriteDo
  , detailsDoConsistentWidth
  , isInconsistentlyIndentedAfter
  ) where

#include <imports/external.hs>
#include <imports/io.hs>

import           Education.MakeMistakesToLearnHaskell.Env
import           Education.MakeMistakesToLearnHaskell.Evaluator.Ghc
import           Education.MakeMistakesToLearnHaskell.Evaluator.Command
import           Education.MakeMistakesToLearnHaskell.Evaluator.Types
import           Education.MakeMistakesToLearnHaskell.Exercise.Types
import           Education.MakeMistakesToLearnHaskell.Text

runHaskellExerciseEq
  :: Diagnosis
  -> Text
  -> Env
  -> FilePath
  -> IO Result
runHaskellExerciseEq diag right e prgFile = do
  result <- runHaskell e prgFile
  code <- readUtf8File prgFile
  return $ resultForUserEq diag code [] (const right) "" result

-- | 'runHaskellExercise' with input to stdin
runHaskellExerciseWithStdinEq
  :: Diagnosis
  -> Gen Text
  -> (Text -> Text)
  -> Env
  -> FilePath
  -> IO Result
runHaskellExerciseWithStdinEq diag gen calcRight env prgFile = do
  let qcArgs = QuickCheck.stdArgs { QuickCheck.chatty = True }
      maxSuccessSize = envQcMaxSuccessSize env

  resultRef <- newIORef $ error "Assertion failure: no result written after QuickCheck"
  code <- readUtf8File prgFile
  Temp.withSystemTempDirectory "mmlh-compiled-answer" $ \dir -> do
    let prg = FilePath.takeBaseName prgFile
        -- TODO: Hide `-o` option as implementation detail
        ghcParams = ["-o", dir </> prg, prgFile]
    exePathOrErr <- compileWithGhc env ghcParams
    case exePathOrErr of
        Right exePath -> do
          qr <- quickCheckWithResult qcArgs $
            QuickCheck.withMaxSuccess maxSuccessSize $
              QuickCheck.forAll gen $ \input ->
                QuickCheck.ioProperty $ do
                  let params = CommandParameters
                        { commandParametersArgs = []
                        , commandParametersStdin = TextEncoding.encodeUtf8 input
                        }
                  commandResult <- executeCommand env prg [exePath] params
                  let messageFooter = ["            For input: " <> Text.pack (show input)]
                      result = resultForUserEq diag code messageFooter calcRight input (Right commandResult)
                  writeIORef resultRef result
                  return $
                    case result of
                        Success _ -> True
                        _other -> False
          logDebug env $ ByteString.pack $ "QuickCheck result: " ++ show qr
          readIORef resultRef
        Left msg ->
          let textMsg = decodeUtf8 msg
           in return . Fail code . CompileError textMsg $ diag code textMsg

resultForUserEq
  :: Diagnosis
  -> Text
  -> [Text]
  -> (Text -> Text)
  -> Text
  -> Either GhcError CommandResult
  -> Result
resultForUserEq diag code messageFooter calcRight input =
  resultForUser diag code messageFooter judge input
 where
  judge acutalOut _err =
    let expectedOut = calcRight input
     in (expectedOut, acutalOut == expectedOut)

resultForUser
  :: Diagnosis
  -> Text
  -> [Text]
  -> Judge
  -> Text
  -> Either GhcError ByteString
  -> Result
resultForUser diag code messageFooter judge input result =
  case result of
      Right outB ->
        let out = canonicalizeNewlines outB
            -- TODO: Merge stdout and stderr from the user's answer
            -- err = canonicalizeNewlines errB
            (right, isSuccessful) = judge input out -- err
            msg =
              Text.unlines $
                [ Text.replicate 80 "="
                , "Your program's output: " <> Text.pack (show out) -- TODO: pretty print
                , "      Expected output: " <> Text.pack (show right)
                ] ++ messageFooter
        in
          if isSuccessful -- out == right
            then Success $ "Nice output!\n\n" <> msg
            else Fail code . WrongOutput $ "Wrong output!\n\n" <> msg
      Left GhcNotFound ->
        Error $ Text.pack "ghc command is not available.\nInstall stack or Haskell Platform."
      Left (GhcError _ msg) ->
        let textMsg = decodeUtf8 msg
         in Fail code . CompileError textMsg $ diag code textMsg

isInWords :: Text -> [Text] -> Bool
isInWords wd = any (Text.isInfixOf wd)

detailsForgetToWriteDo :: Text -> Details
detailsForgetToWriteDo funcNames =
  "HINT: You seem to forget to write `do`. `do` must be put before listing " <> funcNames <> "."

detailsDoConsistentWidth :: Details
detailsDoConsistentWidth = "HINT: instructions in a `do` must be in a consistent width."

isInconsistentlyIndentedAfter :: SourceCode -> Text -> Bool
isInconsistentlyIndentedAfter code wd =
  not
    $ allSame
    $ map (Text.length . Text.takeWhile Char.isSpace)
    $ cropAfterWord wd
    $ Text.lines code
  where
    cropAfterWord :: Text -> [SourceCode] -> [SourceCode]
    cropAfterWord w ls =
      -- Against my expectaion,
      -- 'dropWhile (isInWords w . Text.words) ls' returns ls as is.
      -- While this function should return an empty list
      -- if 'ls' doesn't contain 'w'.
      let (_nonContaining, containing) = List.break (isInWords w . Text.words) ls
      in
        if null containing
          then []
          else drop 1 containing -- except the first line, which contains 'w'

allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame [_] = True
allSame (x1 : x2 : xs) = x1 == x2 && allSame xs

noVeirificationExercise :: Env -> String -> IO Result
noVeirificationExercise _ _ = return NotVerified

notYetImplementedVeirificationExercise :: Env -> FilePath -> IO Result
notYetImplementedVeirificationExercise e prgFile = do
  code <- readUtf8File prgFile
  result <- checkWithGhc e prgFile
  case result of
      Right _ ->
        return NotYetImplemented
      Left (GhcError _ecode msg) ->
        return . Fail code $ CompileError (decodeUtf8 msg) ""
      Left GhcNotFound ->
        return . Error $ Text.pack "ghc command is not available.\nInstall stack or Haskell Platform."
