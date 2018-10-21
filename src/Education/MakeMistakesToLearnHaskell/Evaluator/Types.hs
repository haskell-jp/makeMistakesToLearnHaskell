{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Evaluator.Types
  ( ErrorCode
  , ErrorMessage
  , RunHaskellError(..)
  , SingleArgFunApp(..)
  , HasParens(..)
  ) where


#include <imports/external.hs>


type ErrorCode = Int
type ErrorMessage = ByteString


data RunHaskellError =
  RunHaskellNotFound | RunHaskellFailure ErrorCode ErrorMessage deriving (Show, Typeable)

instance Exception RunHaskellError

data HasParens =
  NoParens | OnlyOpenParen | BothParens
  deriving (Eq, Show)


data SingleArgFunApp = SingleArgFunApp
  { singleArgFunAppFunName :: !TextS.Text
  , singleArgFunAppArg :: !(Maybe SingleArgFunApp)
  , singleArgFunAppHasParen :: !HasParens
  } deriving (Eq, Show)
