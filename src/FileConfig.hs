{-# LANGUAGE DeriveGeneric #-}

module FileConfig where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Control.Applicative ((<|>))
import Control.Monad.Catch
import Control.Monad.Extra

import Data.Aeson as Json
import Data.Yaml as Yaml

import GHC.Generics

import System.Directory
import System.Environment.XDG.BaseDir

import Safe (headMay)

import Util

data FileConfig = FileConfig { tcUrl     :: String
                             , user      :: T.Text
                             , pass      :: T.Text
                             , targetDir :: FilePath
                             , excepts   :: M.Map T.Text FilePath }
    deriving (Generic)
instance FromJSON FileConfig


readFileConfig :: Maybe String -> IO FileConfig
readFileConfig configOverride = do
    configs <- filterM doesFileExist =<< getAllConfigFiles "TCD" "config.yml"
    config  <- maybe (quitError "No config found") return $ configOverride <|> headMay configs

    either throwM return =<< Yaml.decodeFileEither config

