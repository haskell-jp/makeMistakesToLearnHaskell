{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Text
  ( canonicalizeNewlines
  , decodeUtf8
  ) where


#include <imports/external.hs>


canonicalizeNewlines :: ByteString -> Text
canonicalizeNewlines = Text.replace "\r\n" "\n" . decodeUtf8


decodeUtf8 :: ByteString -> Text
decodeUtf8 = TextEncoding.decodeUtf8With handler
  where
    handler _ (Just _) = Just ' '
    handler cause nothing =
      throw $ TextEncoding.DecodeError cause nothing
