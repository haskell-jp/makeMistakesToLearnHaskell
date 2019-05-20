{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Core
  ( runHaskellExercise
  , runHaskellExerciseWithStdin
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
import qualified Education.MakeMistakesToLearnHaskell.Evaluator.Command as Command
import           Education.MakeMistakesToLearnHaskell.Evaluator.RunHaskell
import           Education.MakeMistakesToLearnHaskell.Evaluator.Types
import           Education.MakeMistakesToLearnHaskell.Exercise.Types
import           Education.MakeMistakesToLearnHaskell.Text

runHaskellExercise
  :: Diagnosis
  -> Text
  -> Env
  -> FilePath
  -> IO Result
runHaskellExercise = runHaskellExercise' defaultRunHaskellParameters

runHaskellExercise'
  :: CommandParameters
  -> Diagnosis
  -> Text
  -> Env
  -> FilePath
  -> IO Result
runHaskellExercise' params diag right e prgFile = do
  code <- readUtf8File prgFile
  result <- runHaskell e params { commandParametersArgs = [prgFile] }
  return $ resultForUser diag code [] (const right) "" result

-- runHaskellExercise の入力有りバージョン
runHaskellExerciseWithStdin
  :: Diagnosis
  -> Gen String
  -> (Text -> Text)
  -> Env
  -> FilePath
  -> IO Result
runHaskellExerciseWithStdin diag gen calcRight env prgFile = do
  let qcArgs = QuickCheck.stdArgs { QuickCheck.chatty = True }
      maxSuccessSize = envQcMaxSuccessSize env

  resultRef <- newIORef $ error "Assertion failure: no result written after QuickCheck"
  qr <- quickCheckWithResult qcArgs $
    QuickCheck.forAll gen $ \inputS ->
      QuickCheck.withMaxSuccess maxSuccessSize $
        QuickCheck.ioProperty $ do
          let input = Text.pack inputS
              params = defaultRunHaskellParameters
                { commandParametersArgs = [prgFile]
                , commandParametersStdin = TextEncoding.encodeUtf8 input
                }
          code <- readUtf8File prgFile
          result <- resultForUser diag code ["            For input: " <> Text.pack (show input)] calcRight input <$> runHaskell env params
          writeIORef resultRef result
          return $
            case result of
                Success _ -> True
                _other -> False
  logDebug env $ ByteString.pack $ "QuickCheck result: " ++ show qr
  readIORef resultRef

resultForUser
  :: Diagnosis
  -> Text
  -> [Text]
  -> (Text -> Text)
  -> Text
  -> Either CommandError (ByteString, ByteString)
  -> Result
resultForUser _diag code messageFooter calcRight input (Right (outB, _errB {- TODO: print stderr -})) =
  let out = canonicalizeNewlines outB
      right = calcRight input
      msg =
        Text.unlines $
          [ Text.replicate 80 "="
          , "Your program's output: " <> Text.pack (show out) -- TODO: pretty print
          , "      Expected output: " <> Text.pack (show right)
          ] ++ messageFooter
  in
    if right == out
      then Success $ "Nice output!\n\n" <> msg
      else Fail code . WrongOutput $ "Wrong output!\n\n" <> msg
resultForUser _diag _code _messageFooter _calcRight _minput (Left (Command.CommandNotFound cname)) =
  Error $ Text.pack cname <> " command is not available.\nInstall stack or Haskell Platform."
resultForUser diag code _messageFooter _calcRight _minput (Left (Command.CommandFailure cname _ msg)) =
  let textMsg = decodeUtf8 msg
   in Fail code . CommandFailed cname textMsg $ diag code textMsg

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
  result <- runGhc e defaultRunHaskellParameters { commandParametersArgs = [prgFile] }
  case result of
      Right _ ->
        return NotYetImplemented
      Left (CommandFailure cname _ecode msg) ->
        return . Fail code $ CommandFailed cname (decodeUtf8 msg) ""
      Left (CommandNotFound cname) ->
        return . Error $ Text.pack cname <> " command is not available.\nInstall stack or Haskell Platform."
