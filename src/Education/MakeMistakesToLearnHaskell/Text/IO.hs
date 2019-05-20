{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeMistakesToLearnHaskell.Text.IO
  ( readUtf8File
  , writeUtf8FileS
  ) where



#include <imports/external.hs>
#include <imports/external/io.hs>


readUtf8File :: FilePath -> IO Text
readUtf8File path = do
  hd <- IO.openFile path IO.ReadMode
  IO.hSetEncoding hd IO.utf8
  Text.hGetContents hd


writeUtf8FileS :: FilePath -> TextS.Text -> IO ()
writeUtf8FileS path dat =
  IO.withFile path IO.WriteMode $ \hd -> do
    IO.hSetEncoding hd IO.utf8
    TextS.hPutStr hd dat
