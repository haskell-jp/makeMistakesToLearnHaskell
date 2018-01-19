{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeThemToLearnHaskell.Evaluator.Util
  ( canonicalizeOutput
  ) where


#include <imports/external.hs>


canonicalizeOutput :: ByteString -> Text
canonicalizeOutput =
  Text.replace "\r\n" "\n" . TextEncoding.decodeUtf8
