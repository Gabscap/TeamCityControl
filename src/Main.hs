{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Text as T

import Control.Exception
import Data.List (isPrefixOf)
import Prelude hiding (catch)
import Options.Applicative

import FileConfig
import OptParse
import TCDownloader

main :: IO ()
main = do
    Options{..} <- parseCLI projectNameCompleter
    fc          <- readFileConfig oConfig

    config <- freshConfig fc (T.pack oProject) (T.pack <$> oBuild) oDebug
    runTCM mainProgram config

projectNameCompleter :: Completer
projectNameCompleter = mkCompleter $ \search -> do
    fc <- readFileConfig Nothing

    config <- freshConfig fc T.empty Nothing False

    names <- (runTCM listProjectNames config)
        `catch` (\(_ :: SomeException) -> return [])
    return . filter (isPrefixOf search) $ T.unpack <$> names
