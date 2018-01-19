module Util where

import qualified Data.Text as T

import Control.Monad.IO.Class

import System.Exit
import System.IO


quitError :: MonadIO m => String -> m a
quitError err = liftIO $ hPutStrLn stderr err >> exitFailure

tShow :: Show a => a -> T.Text
tShow = T.pack . show
