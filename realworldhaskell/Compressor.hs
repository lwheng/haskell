import           Control.Concurrent (forkIO)
import           Control.Exception (handle, SomeException)
import           Control.Monad (forever)
import           System.Console.Haskeline (runInputT, defaultSettings, getInputLine)
import qualified Data.ByteString.Lazy as L

-- Provided by the 'zlib' package on http://hackage.haskell.org/
import           Codec.Compression.GZip (compress)

main :: IO ()
main = do
  maybeLine <- runInputT defaultSettings $ getInputLine "Enter a file to compress> "
  case maybeLine of
    Nothing -> return () -- user entered EOF
    Just "" -> return () -- treat no name as "want to quit"
    Just name -> do
      handle (print :: SomeException -> IO ()) $ do
        content <- L.readFile name
        forkIO (compressFile name content)
        return ()
      main
  where compressFile path = L.writeFile (path ++ ".gz") . compress
