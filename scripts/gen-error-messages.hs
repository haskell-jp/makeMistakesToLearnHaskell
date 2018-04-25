import           Control.Monad (forM_, void)
import qualified Data.ByteString as ByteString
import           Data.Function ((&))
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.FilePath as Path
import           System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.Process.Typed as Process


main :: IO ()
main = do
  files <- Env.getArgs
  forM_ files $ \file -> do
    let outDir = Path.takeDirectory file </> "error-messages"
    Dir.createDirectoryIfMissing True outDir

    IO.withFile (outDir </> Path.takeBaseName file ++ ".txt") IO.WriteMode $ \hd -> do
      let p =
            Process.proc "stack" ["exec", "ghc", file]
              & Process.setStderr (Process.useHandleClose hd)
              & Process.setStdout (Process.useHandleClose hd)
      void $ Process.runProcess p
