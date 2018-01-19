{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module TCResponse where

import qualified Data.Text as T

import Data.Aeson
import Data.Time.Clock
import Data.Time.Format
import GHC.Generics

data Projects =
    Projects { count   :: Int
             , href    :: Maybe T.Text
             , project :: [Project] }
    deriving (Show, Generic)
instance FromJSON Projects

data Project =
    Project { id              :: T.Text
            , name            :: T.Text
            , description     :: Maybe T.Text
            , parentProjectId :: Maybe T.Text
            , href            :: T.Text
            , webUrl          :: T.Text }
    deriving (Show, Generic)
instance FromJSON Project

data ProjectInfo =
    ProjectInfo { id              :: T.Text
                , name            :: T.Text
                , parentProjectId :: T.Text
                , href            :: T.Text
                , webUrl          :: T.Text
                , parentProject   :: Project
                , buildTypes      :: BuildTypes }
    deriving (Show, Generic)
instance FromJSON ProjectInfo

data BuildTypes =
    BuildTypes { count     :: Int
               , buildType :: [BuildType] }
    deriving (Show, Generic)
instance FromJSON BuildTypes

data BuildType =
    BuildType { id          :: T.Text
              , name        :: T.Text
              , projectName :: T.Text
              , projectId   :: T.Text
              , href        :: T.Text
              , webUrl      :: T.Text }
    deriving (Show, Generic)
instance FromJSON BuildType

data Files =
    Files { count :: Int
          , file  :: [File] }
    deriving (Show, Generic)
instance FromJSON Files

data File =
    File { name     :: T.Text
         , href     :: T.Text
         , children :: Maybe Href
         , content  :: Maybe Href }
    deriving (Show, Generic)
instance FromJSON File

data Href =
    Href { href :: T.Text }
    deriving (Show, Generic)
instance FromJSON Href

data Builds =
    Builds { count :: Int
           , build :: [Build] }
    deriving (Show, Generic)
instance FromJSON Builds

data Build =
    Build { id     :: Int
          , number :: T.Text
          , status :: T.Text
          , state  :: T.Text
          , href   :: T.Text }
    deriving (Show, Generic)
instance FromJSON Build

data BuildInfo =
    BuildInfo { id         :: Int
              , finishDate :: UTCTime }
    deriving (Show, Generic)
instance FromJSON BuildInfo where
    parseJSON = withObject "BuildInfo" $ \o ->
        BuildInfo <$> o .: "id"
                  <*> (readTCTime <$> o .: "finishDate")

readTCTime :: ParseTime t => String -> t
readTCTime = parseTimeOrError True defaultTimeLocale "%Y%m%dT%H%M%S%z"
