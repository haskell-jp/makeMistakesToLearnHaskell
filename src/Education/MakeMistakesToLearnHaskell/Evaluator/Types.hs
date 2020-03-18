{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Evaluator.Types
  ( ErrorCode
  , ErrorMessage
  , CommandName
  , CommandResult(..)
  , GhcError(..)
  , SingleArgFunApp(..)
  , HasParens(..)
  ) where


#include <imports/external.hs>


type CommandName = String
type ErrorCode = Int
type ErrorMessage = ByteString


data CommandResult =
  CommandResult
    !ExitCode   -- ^ exit code
    !ByteString -- ^ Merged stdout and stderr
    deriving Show


data GhcError =
  GhcNotFound | GhcError ErrorCode ErrorMessage deriving Show

instance Exception GhcError

data HasParens =
  NoParens | OnlyOpenParen | BothParens
  deriving (Eq, Show)


data SingleArgFunApp = SingleArgFunApp
  { singleArgFunAppFunName :: !TextS.Text
  , singleArgFunAppArg :: !(Maybe SingleArgFunApp)
  , singleArgFunAppHasParen :: !HasParens
  } deriving (Eq, Show)
