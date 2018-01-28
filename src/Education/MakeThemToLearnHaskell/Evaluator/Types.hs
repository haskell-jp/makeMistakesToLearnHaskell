{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeThemToLearnHaskell.Evaluator.Types
  ( ErrorCode
  , ErrorMessage
  , RunHaskellError(..)
  ) where


#include <imports/external.hs>


type ErrorCode = Int
type ErrorMessage = ByteString


data RunHaskellError =
  RunHaskellNotFound | RunHaskellFailure ErrorCode ErrorMessage deriving (Show, Typeable)

instance Exception RunHaskellError
