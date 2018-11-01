{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise
  ( Exercise(verify)
  , Name
  , Result(..)
  , Details
  , loadHeaders
  , loadDescriptionByName
  , loadExampleSolution
  , loadLastShown
  , saveLastShownName
  , unsafeGetByName
  ) where


#include <imports/external.hs>

import qualified Paths_makeMistakesToLearnHaskell

import           Education.MakeMistakesToLearnHaskell.Diagnosis
import           Education.MakeMistakesToLearnHaskell.Env
import           Education.MakeMistakesToLearnHaskell.Evaluator.Regex
import qualified Education.MakeMistakesToLearnHaskell.Evaluator.RunHaskell as RunHaskell
import           Education.MakeMistakesToLearnHaskell.Evaluator.Types
import           Education.MakeMistakesToLearnHaskell.Exercise.FormatMessage
import           Education.MakeMistakesToLearnHaskell.Exercise.Record
import           Education.MakeMistakesToLearnHaskell.Exercise.Types
import           Education.MakeMistakesToLearnHaskell.Error
import           Education.MakeMistakesToLearnHaskell.Text


exercises :: [(Name, Exercise)]
exercises = map (\e -> (exerciseName e, e))
    [ exercise1
    , exercise2
    , exercise2_5
    , exercise3
    , exercise4
    , exercise5
    ]
  where
    exercise1 =
      Exercise "1" $ runHaskellExercise diag1 "Hello, world!\n"

    diag1 :: Diagnosis
    diag1 code msg
      | "parse error on input" `Text.isInfixOf` msg
          && "'" `Text.isInfixOf` code =
            "HINT: In Haskell, you must surround string literals with double-quote '\"'. Such as \"Hello, world\"."
      | ("parse error" `Text.isInfixOf` msg || "Parse error" `Text.isInfixOf` msg)
          && "top-level declaration expected." `Text.isInfixOf` msg =
            "HINT: This error indicates you haven't defined main function."
      | "Variable not in scope: main :: IO" `Text.isInfixOf` msg =
        "HINT: This error indicates you haven't defined main function."
      | "Variable not in scope:" `Text.isInfixOf` msg =
        "HINT: you might have misspelled 'putStrLn'."
      | otherwise = ""

    exercise2 =
      Exercise "2" $ runHaskellExercise diag2 "20.761245674740486\n"

    diag2 :: Diagnosis
    diag2 code msg
      | "parse error" `Text.isInfixOf` msg || "Parse error" `Text.isInfixOf` msg =
        if "top-level declaration expected." `Text.isInfixOf` msg
          then
            "HINT: This error indicates you haven't defined main function."
          else
            -- TODO: Use regex or ghc tokenizer
            case compare (Text.count "(" code) (Text.count ")" code) of
                GT -> "HINT: you might have forgot to write close parenthesis"
                LT -> "HINT: you might have forgot to write open parenthesis"
                EQ -> ""
      | "No instance for (Fractional (IO ()))" `Text.isInfixOf` msg || "No instance for (Num (IO ()))" `Text.isInfixOf` msg =
        "HINT: you might have forgot to write parentheses"
      | "No instance for (Show (a0 -> a0))" `Text.isInfixOf` msg =
        "HINT: you might have forgot to write some numbers between operators ('*', '/' etc.)."
      | "No instance for (Num (t0 -> a0))" `Text.isInfixOf` msg =
        "HINT: you might have forgot to write multiplication operator '*'"
      | "No instance for (Fractional (t0 -> a0))" `Text.isInfixOf` msg =
        "HINT: you might have forgot to write division operator '/'"
      | "Variable not in scope: main :: IO" `Text.isInfixOf` msg =
        "HINT: This error indicates you haven't defined main function."
      | otherwise = ""

    exercise2_5 = Exercise "2.5" noVeirificationExercise

    exercise3 =
      Exercise "3" $ runHaskellExercise diag3 $ Text.unlines
        [ "#     # ####### #       #        #####"
        , "#     # #       #       #       #     #"
        , "#     # #       #       #       #     #"
        , "####### #####   #       #       #     #"
        , "#     # #       #       #       #     #"
        , "#     # #       #       #       #     #"
        , "#     # ####### ####### #######  #####"
        ]

    diag3 :: Diagnosis
    diag3 code msg
      | code `isInconsistentlyIndentedAfter` "do" = detailsDoConsistentWidth
      | "parse error on input" `Text.isInfixOf` msg
          && "'" `Text.isInfixOf` code =
            "HINT: In Haskell, you must surround string literals with double-quote '\"'. Such as \"Hello, world\"."
      | ("parse error" `Text.isInfixOf` msg || "Parse error" `Text.isInfixOf` msg)
          && "top-level declaration expected." `Text.isInfixOf` msg =
            "HINT: This error indicates you haven't defined main function."
      | "Variable not in scope: main :: IO" `Text.isInfixOf` msg =
        "HINT: This error indicates you haven't defined main function."
      | "Variable not in scope:" `Text.isInfixOf` msg =
        "HINT: you might have misspelled 'putStrLn'."
      | "Couldn't match expected type ‘(String -> IO ())" `Text.isInfixOf` msg =
          detailsForgetToWriteDo "`putStrLn`s"
      | otherwise = ""

    exercise4 = Exercise "4"
              $ runHaskellExerciseWithStdin diag4 gen4
              $ (Text.pack . unlines . reverse . lines . Text.unpack)

    gen4 :: (Gen [PrintableString], [PrintableString] -> [String])
    gen4 = (arbitrary, map QuickCheck.getPrintableString)

    diag4 :: Diagnosis
    diag4 code msg
      | code `isInconsistentlyIndentedAfter` "do" =
        detailsDoConsistentWidth
      | "Perhaps this statement should be within a 'do' block?" `Text.isInfixOf` msg =
        if hasNoMainFirst code then
          "HINT: Your source code dosn't have `main` function!"
          -- ^ TODO: Rewrite other no-main cases with this.
        else if code `containsSequence` ["main", "<-"] then
          "HINT: Don't use `<-` to define the `main` function. Use `=` instead."
        else
          detailsForgetToWriteDo "`putStr`s and `getContents`"
      | "Perhaps you need a 'let' in a 'do' block?" `Text.isInfixOf` msg
        && code `containsSequence` ["=", "getContents"] =
          "HINT: Don't assign the result of `getContents` with `=`. Use `<-` instead."
      | "Couldn't match type ‘IO String’ with ‘[Char]’" `Text.isInfixOf` msg
        && "In the first argument of ‘lines’" `Text.isInfixOf` msg =
          "HINT: Unfortunately, you have to assign the result of `getContents` with `<-` operator."
      | otherwise =
        let mtoks = GHC.tokenizeHaskell (Text.toStrict code)
            tokPutStr = (GHC.VariableTok, "putStr")
            putStrThenSpace =
              Regex.sym tokPutStr <* optional (Regex.psym ((== GHC.SpaceTok) . fst))
            msafa =
              matchSub (singleArgFunApp 5) . (tokPutStr :) . dropUntilFirst putStrThenSpace =<< mtoks
        in
          case msafa of
              Just safa -> Text.unlines $ formatSingleArgFunApp safa
              _ -> ""

    exercise5 = Exercise "5"
              $ runHaskellExerciseWithStdin diag5 gen5 ans5

    diag5 :: Diagnosis
    diag5 code msg = ""

    gen5 :: (Gen Exercise5, Exercise5 -> [String])
    gen5 = (arbitrary, ex5ToString)

    ans5 :: Text -> Text
    ans5 input = Text.pack $ show body <> "\n"
      where
        [principal, interestRate, years] = lines $ Text.unpack input
        body = read principal * (1 + read interestRate / 100) ^ read years

    detailsForgetToWriteDo :: Text -> Details
    detailsForgetToWriteDo funcNames =
      "HINT: You seem to forget to write `do`. `do` must be put before listing " <> funcNames <> "."


    detailsDoConsistentWidth :: Details
    detailsDoConsistentWidth = "HINT: instructions in a `do` must be in a consistent width."

data Exercise5 = Exercise5 Double Double (Positive Int)
  deriving Show

instance Arbitrary Exercise5 where
  arbitrary = Exercise5
           <$> (arbitrary :: Gen Double)
           <*> (arbitrary :: Gen Double)
           <*> (arbitrary :: Gen (Positive Int))

ex5ToString :: Exercise5 -> [String]
ex5ToString (Exercise5 a b c) =
  [ show a
  , show b
  , show $ QuickCheck.getPositive c
  ]

-- TODO: Incomplete implementaion! Use regex or ghc tokenizer!
containsSequence :: SourceCode -> [Text] -> Bool
containsSequence code wds =
  Text.concat wds `isInWords` ws || (wds `List.isInfixOf` ws)
  where
    ws = Text.words code


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
          else drop 1 containing
          -- ^ except the first line, which contains 'w'


isInWords :: Text -> [Text] -> Bool
isInWords wd = any (Text.isInfixOf wd)


allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame [_] = True
allSame (x1 : x2 : xs) = x1 == x2 && allSame xs


hasNoMainFirst :: SourceCode -> Bool
hasNoMainFirst src =
  case Text.words src of
      [] -> True
      (h : _) -> not $ "main" `Text.isPrefixOf` h

runHaskellExercise
  :: Diagnosis
  -> Text
  -> Env
  -> FilePath
  -> IO Result
runHaskellExercise = runHaskellExercise' Nothing

-- TODO: refactor with resultForUser
runHaskellExercise'
  :: Maybe RunHaskellParameters
  -> Diagnosis
  -> Text
  -> Env
  -> FilePath
  -> IO Result
runHaskellExercise' mParam diag right e prgFile = do
  let rhp = fromMaybe defaultRunHaskellParameters mParam
  result <- runHaskell e $ rhp { runHaskellParametersArgs = [prgFile] }
  case result of
      Right (outB, _errB {- TODO: print stderr -}) -> do
        let out = canonicalizeNewlines outB
            msg =
              Text.unlines
                [ Text.replicate 80 "="
                , "Your program's output: " <> Text.pack (show out) -- TODO: pretty print
                , "      Expected output: " <> Text.pack (show right)
                ]
        return $
          if out == right
            then Success $ "Nice output!\n\n" <> msg
            else Fail $ "Wrong output!\n\n" <> msg
      Left err -> do
        case err of
            RunHaskell.RunHaskellNotFound ->
              return $ Error "runhaskell command is not available.\nInstall stack or Haskell Platform."
            RunHaskell.RunHaskellFailure _ msg -> do
              logDebug e $ "RunHaskellFailure: " <> msg
              code <- readUtf8File prgFile
              return $ Fail $ appendDiagnosis diag code msg

runHaskellExerciseWithStdin
  :: Show a
  => Diagnosis
  -> (Gen a, a -> [String])
  -> (Text -> Text)
  -> Env
  -> FilePath
  -> IO Result
runHaskellExerciseWithStdin diag (gen, toInputString) calcRight env prgFile = do
  let qcArgs = QuickCheck.stdArgs { QuickCheck.chatty = True }
      maxSuccessSize = envQcMaxSuccessSize env

  resultRef <- newIORef $ error "Assertion failure: no result written after QuickCheck"
  qr <- quickCheckWithResult qcArgs $
    QuickCheck.forAll gen $ \inputArgs ->
      QuickCheck.withMaxSuccess maxSuccessSize $
        QuickCheck.ioProperty $ do
          let input = Text.pack $ unlines $ toInputString inputArgs
              params = defaultRunHaskellParameters
                { runHaskellParametersArgs = [prgFile]
                , runHaskellParametersStdin = TextEncoding.encodeUtf8 input
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


resultForUser :: Diagnosis -> Text -> [Text] -> (Text -> Text) -> Text -> Either RunHaskellError (ByteString, ByteString) -> Result
resultForUser _diag _code messageFooter calcRight input (Right (outB, _errB {- TODO: print stderr -})) =
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
      else Fail $ "Wrong output!\n\n" <> msg
resultForUser _diag _code _messageFooter _calcRight _minput (Left RunHaskell.RunHaskellNotFound) =
  Error "runhaskell command is not available.\nInstall stack or Haskell Platform."
resultForUser diag code _messageFooter _calcRight _minput (Left (RunHaskell.RunHaskellFailure _ msg)) =
  Fail $ appendDiagnosis diag code msg


noVeirificationExercise :: Env -> String -> IO Result
noVeirificationExercise _ _ = return NotVerified


notYetImplementedVeirificationExercise :: Env -> String -> IO Result
notYetImplementedVeirificationExercise _ _ = return NotYetImplemented


loadHeaders :: IO [Text]
loadHeaders = mapM (loadHeader . snd) exercises
  where
    loadHeader ex = extractHeader ex =<< loadDescription ex

    extractHeader ex desc =
      dieWhenNothing ("The description of exercise '" ++ exerciseName ex ++ "' is empty!")
        $ appendName ex . cutHash <$> headMay (Text.lines desc)

    cutHash h =
      Text.strip $ fromMaybe h $ Text.stripPrefix "# " h

    appendName ex h =
      Text.pack (exerciseName ex) <> ": " <> h


loadDescription :: Exercise -> IO Text
loadDescription = loadWithExtension ".md"


loadExampleSolution :: Exercise -> IO Text
loadExampleSolution = loadWithExtension ".hs"


loadWithExtension :: String -> Exercise -> IO Text
loadWithExtension ext ex =
  Paths_makeMistakesToLearnHaskell.getDataFileName ("assets/" ++ exerciseName ex ++ ext)
    >>= readUtf8File


loadDescriptionByName :: Name -> IO (Maybe Text)
loadDescriptionByName n = MaybeT.runMaybeT $ do
  ex <- Error.hoistMaybe $ getByName n
  liftIO $ loadDescription ex


-- Handle error internally.
-- Because lastShownName is usually saved internally.
loadLastShown :: Env -> IO Exercise
loadLastShown e =
  loadLastShownName e >>=
    dieWhenNothing "Assertion failure: Invalid lastShownName saved! " . getByName


getByName :: Name -> Maybe Exercise
getByName n = lookup n exercises


unsafeGetByName :: Name -> Exercise
unsafeGetByName = fromMaybe (error "unsafeGetByName: No exercise found!") . getByName
