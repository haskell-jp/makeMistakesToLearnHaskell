{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Core
  ( runHaskellExerciseEq
  , runHaskellExerciseWithStdinEq
  , runHaskellExerciseWithStdin
  , runHaskellExerciseWithArgsEq
  , runHaskellExerciseWithArgs
  , runHaskellExerciseWithArgsAndStdin
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
  let calcRight = const right
  return $ resultForUser diag code [] (stdinJudgeByEq calcRight) [] "" result


-- | 'runHaskellExercise' with input to stdin
runHaskellExerciseWithStdinEq
  :: Diagnosis
  -> (Text -> Text)
  -> Gen Text
  -> Env
  -> FilePath
  -> IO Result
runHaskellExerciseWithStdinEq diag =
  runHaskellExerciseWithStdin diag . stdinJudgeByEq


-- Currently the command line arguments and exit code are ignored.
-- If you want to judge by the exit code, consider adding an error message
-- when the acutalOut is correct, but the exit code is wrong.
stdinJudgeByEq :: (Text -> Text) -> Judge
stdinJudgeByEq calcRight _args input _ecode acutalOut =
  let expectedOut = calcRight input
   in (expectedOut, acutalOut == expectedOut)


argsJudgeByEq :: ([CommandLineArg] -> Text) -> Judge
argsJudgeByEq calcRight args _input _ecode acutalOut =
  let expectedOut = calcRight args
   in (expectedOut, acutalOut == expectedOut)


runHaskellExerciseWithStdin
  :: Diagnosis
  -> Judge
  -> Gen Text
  -> Env
  -> FilePath
  -> IO Result
runHaskellExerciseWithStdin diag judge =
  runHaskellExerciseWithArgsAndStdin diag judge $ pure []


runHaskellExerciseWithArgsEq
  :: Diagnosis
  -> ([CommandLineArg] -> Text)
  -> Gen [CommandLineArg]
  -> Education.MakeMistakesToLearnHaskell.Env.Env
  -> String
  -> IO Result
runHaskellExerciseWithArgsEq diag =
  runHaskellExerciseWithArgs diag . argsJudgeByEq


runHaskellExerciseWithArgs
  :: Diagnosis
  -> Judge
  -> Gen [CommandLineArg]
  -> Env
  -> FilePath
  -> IO Result
runHaskellExerciseWithArgs diag judge genArgs =
  runHaskellExerciseWithArgsAndStdin diag judge genArgs (pure "")


runHaskellExerciseWithArgsAndStdin
  :: Diagnosis
  -> Judge
  -> Gen [CommandLineArg]
  -> Gen Text
  -> Env
  -> FilePath
  -> IO Result
runHaskellExerciseWithArgsAndStdin diag judge genArgs genInput env prgFile = do
  let qcArgs = QuickCheck.stdArgs { QuickCheck.chatty = True }
      maxSuccessSize = envQcMaxSuccessSize env
      gen = (,) <$> genArgs <*> genInput

  resultRef <- newIORef $ error "Assertion failure: no result written after QuickCheck"
  code <- readUtf8File prgFile
  Temp.withSystemTempDirectory "mmlh-compiled-answer" $ \dir -> do
    exePathOrErr <- compileWithGhc env dir prgFile
    case exePathOrErr of
        Right exePath -> do
          qr <- quickCheckWithResult qcArgs $
            QuickCheck.withMaxSuccess maxSuccessSize $
              QuickCheck.forAll gen $ \(args, input) ->
                QuickCheck.ioProperty $ do
                  let params = CommandParameters
                        { commandParametersArgs = map asMereString args
                        , commandParametersStdin = TextEncoding.encodeUtf8 input
                        }
                  -- TODO: Create temporary file for input with generated names
                  commandResult <- executeCommand env exePath params
                  logDebug env $ ByteString.pack (show commandResult)

                  -- TODO: Show the contents of the files of the arguments
                  let messageFooter =
                        [ "            For input: " <> Text.pack (show input)
                        , "            For args: " <> Text.pack (show args)
                        ]
                      result = resultForUser diag code messageFooter judge args input (Right commandResult)
                  writeIORef resultRef result
                  return $
                    case result of
                        Success _ -> True
                        _other -> False
          logDebug env $ ByteString.pack $ "QuickCheck result: " ++ show qr
          readIORef resultRef
        Left (GhcError _exitCode msg) -> -- TODO: duplicate error handling code
          let textMsg = decodeUtf8 msg
           in return . Fail code . CompileError textMsg $ diag code textMsg
        Left GhcNotFound ->
          return . Error $ Text.pack "ghc command is not available.\nInstall stack or Haskell Platform."


resultForUser
  :: Diagnosis
  -> Text
  -> [Text]
  -> Judge
  -> [CommandLineArg]
  -> Text
  -> Either GhcError CommandResult
  -> Result
resultForUser diag code messageFooter judge args input result =
  case result of
      Right (CommandResult ecode outB) ->
        let out = canonicalizeNewlines outB
            (right, isSuccessful) = judge args input ecode out
            msg =
              Text.unlines $
                [ Text.replicate 80 "="
                , "Your program's output: " <> Text.pack (show out) -- TODO: pretty print
                , "      Expected output: " <> Text.pack (show right)
                ] ++ messageFooter
        in
          if isSuccessful
            then Success $ "Nice output!\n\n" <> msg
            else Fail code . WrongOutput $ "Wrong output!\n\n" <> msg
      Left GhcNotFound -> -- TODO: duplicate error handling code
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


-- TODO: Move to a separate module which collects functions specific to `CommandLineArg` type.
asMereString :: CommandLineArg -> String
asMereString (Mere s) = s
asMereString (FilePath path _content) = path


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
