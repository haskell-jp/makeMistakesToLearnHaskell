{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Text
  ( canonicalizeNewlines
  , decodeUtf8
  , removeAllTrailingSpace
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


-- | See https://github.com/haskell-jp/makeMistakesToLearnHaskell/pull/51#issuecomment-435296461
removeAllTrailingSpace :: Text -> Text
removeAllTrailingSpace = Text.unlines . map Text.stripEnd . Text.lines
