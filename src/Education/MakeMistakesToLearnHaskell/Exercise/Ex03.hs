module Education.MakeMistakesToLearnHaskell.Exercise.Ex03
  ( exercise3
  ) where

#include <imports/external.hs>

import Education.MakeMistakesToLearnHaskell.Exercise.Core
import Education.MakeMistakesToLearnHaskell.Exercise.Types

exercise3 :: Exercise
exercise3 = Exercise "3" $ runHaskellExercise diag3 $ Text.unlines
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