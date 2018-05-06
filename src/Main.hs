{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Text as T

import Control.Exception
import Data.List (isPrefixOf)
import Prelude hiding (catch)
import Options.Applicative

import FileConfig
import OptParse
import qualified TCRest as TC

import TCBuild
import TCList
import TCDownload

main :: IO ()
main = do
    Options{..} <- parseCLI projectNameCompleter
    fc          <- readFileConfig oConfig

    case oAction of
        Download DownloadInfo{..} -> do
            config <- freshConfig fc (T.pack oProject) (T.pack <$> oBuild) oDebug
            tcDownload config
        List -> do
            config <- TC.freshConfig fc oDebug
            tcList config
        Build project -> do
            config <- freshBuildConfig fc (T.pack project) oDebug
            tcBuild config

projectNameCompleter :: Completer
projectNameCompleter = mkCompleter $ \search -> do
    fc <- readFileConfig Nothing

    config <- TC.freshConfig fc False

    names <- (TC.runTCMRest TC.listProjectNames config)
        `catch` (\(_ :: SomeException) -> return [])
    return . filter (isPrefixOf search) $ T.unpack <$> names
