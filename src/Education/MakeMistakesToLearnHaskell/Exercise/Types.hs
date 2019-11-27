{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Types
  ( module Education.MakeMistakesToLearnHaskell.Commons.Exercise
  , Exercise (..)
  , Result (..)
  , Record (..)
  , Diagnosis
  ) where

#include <imports/external.hs>

import           Education.MakeMistakesToLearnHaskell.Commons.Exercise
import           Education.MakeMistakesToLearnHaskell.Evaluator.Types
import           Education.MakeMistakesToLearnHaskell.Env

data Exercise =
  Exercise
    { name :: !Name
    -- ^ The name of the exercise.
    , verify :: Env -> String -> IO Result
    -- ^ The function to verify the source file, project directory,
    --   or any string pointing to the user's answer.
    --   So, the second argument's @String@ is something
    --   pointing to the user's answer.
    }


-- | Result of exercise
data Result =
    Error -- ^ Something unexpected has happened.
      !Details -- ^ The details of the error. Usually error messages from the ghc command etc.
  | Fail -- ^ User's answer is wrong (or has type errors).
      !SourceCode -- ^ User's answer as source code.
      !FailBy -- ^ The reason why the answer is wrong.
  | Success -- ^ User's answer is correct.
      !Details
  | NotVerified -- ^ This exercise doesn't have no verification. Go ahead!
  | NotYetImplemented -- ^ Verification is not implemented yet. Sorry! Go ahead!
  deriving (Eq, Show)

newtype Record = Record
  { lastShownName :: Name
  } deriving (Show, Read)

type Diagnosis = SourceCode -> Details -> Details
