{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Exercise.Types where

#include <imports/external.hs>

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


data Result =
    Error !Details
  | Fail !FailBy
  | Success !Details
  | NotVerified
  | NotYetImplemented
  deriving (Eq, Show)

data FailBy =
    WrongOutput !Details
  | CommandFailed
      String   -- ^ Command name
      !Details -- ^ Output by command
      !Details -- ^ Diagnosis message
  deriving (Eq, Show)

newtype Record = Record
  { lastShownName :: Name
  } deriving (Show, Read)

type Details = Text

type SourceCode = Text

type Name = String

type Diagnosis = SourceCode -> Details -> Details
