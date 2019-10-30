{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Evaluator.Types
  ( ErrorCode
  , ErrorMessage
  , CommandName
  , CommandError(..)
  , SingleArgFunApp(..)
  , HasParens(..)
  ) where


#include <imports/external.hs>


type CommandName = String
type ErrorCode = Int
type ErrorMessage = ByteString


data CommandError =
  CommandNotFound CommandName | CommandFailure CommandName ErrorCode ErrorMessage deriving (Show, Typeable)

instance Exception CommandError

data HasParens =
  NoParens | OnlyOpenParen | BothParens
  deriving (Eq, Show)


data SingleArgFunApp = SingleArgFunApp
  { singleArgFunAppFunName :: !TextS.Text
  , singleArgFunAppArg :: !(Maybe SingleArgFunApp)
  , singleArgFunAppHasParen :: !HasParens
  } deriving (Eq, Show)
