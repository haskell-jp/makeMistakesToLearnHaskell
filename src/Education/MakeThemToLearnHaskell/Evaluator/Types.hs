{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeThemToLearnHaskell.Evaluator.Types
  ( ErrorCode
  , ErrorMessage
  ) where


#include <imports/external.hs>


type ErrorCode = Int
type ErrorMessage = ByteString
