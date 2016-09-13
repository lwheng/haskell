{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HandleIO (
    HandleIO
  , Handle
  , IOMode (..)
  , hClose
  , hPutStrLn
  , openFile
  , runHandleIO
) where

import           Control.Monad.Trans (MonadIO (..))
import           System.IO           (Handle, IOMode (..))
import qualified System.IO as SIO

newtype HandleIO a = HandleIO {
                                runHandleIO :: IO a
                              } deriving (Functor, Applicative, Monad)

instance MonadIO HandleIO where
  liftIO = HandleIO

openFile
  :: FilePath
  -> IOMode
  -> HandleIO Handle
openFile path mode = HandleIO (SIO.openFile path mode)

hClose
  :: Handle
  -> HandleIO ()
hClose = HandleIO . SIO.hClose

hPutStrLn
  :: Handle
  -> String
  -> HandleIO ()
hPutStrLn h s = HandleIO (SIO.hPutStrLn h s)

safeHello
  :: FilePath
  -> HandleIO ()
safeHello path = do
  h <- openFile path WriteMode
  hPutStrLn h "Hello World!"
  hClose h
