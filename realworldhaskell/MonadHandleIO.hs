{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

import           Control.Monad.Trans (MonadIO (..), MonadTrans (..))
import           MonadHandle
import           SafeHello
import           System.Directory    (removeFile)
import           System.IO           (IOMode (..))
import qualified System.IO as SIO

instance MonadHandle SIO.Handle IO where
  openFile     = SIO.openFile    
  hPutStr      = SIO.hPutStr     
  hClose       = SIO.hClose      
  hGetContents = SIO.hGetContents
  hPutStrLn    = SIO.hPutStrLn   
  
