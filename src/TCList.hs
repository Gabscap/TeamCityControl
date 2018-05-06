{-# LANGUAGE DuplicateRecordFields #-}

module TCList where

import Control.Monad.IO.Class
import qualified Data.Text.IO as T
import qualified TCRest as TC

type TCMList m = TC.TCMRest m
type ListConfig = TC.RestConfig

runTCMList :: TCMList IO a -> ListConfig -> IO a
runTCMList = TC.runTCMRest

tcList :: ListConfig -> IO ()
tcList = runTCMList tcListM

tcListM :: TCMList IO ()
tcListM = TC.listProjectNames >>= liftIO . mapM_ T.putStrLn

