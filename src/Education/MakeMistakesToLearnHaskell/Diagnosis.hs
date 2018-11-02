{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Diagnosis
  ( appendDiagnosis
  ) where


#include <imports/external.hs>

import           Education.MakeMistakesToLearnHaskell.Text
import           Education.MakeMistakesToLearnHaskell.Evaluator.Types
import           Education.MakeMistakesToLearnHaskell.Exercise.Types


appendDiagnosis :: Diagnosis -> SourceCode -> ErrorMessage -> Details
appendDiagnosis diagFunc srcCode errMsg = Text.intercalate "\n" $
    [ errMsg'
    , "==================== mmlh HINT output ===================="
    , ""
    , diagFunc srcCode errMsg'
    ]
  where
    errMsg' = decodeUtf8 errMsg