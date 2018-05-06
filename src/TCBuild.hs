{-# LANGUAGE DuplicateRecordFields #-}

module TCBuild where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.List
import Data.Maybe
import Data.Semigroup ((<>))

import Network.HTTP.Conduit

import FileConfig
import qualified TCRest as TC
import Util

type TCMBuild m = TC.TCM BuildConfig m

data BuildConfig =
    BuildConfig { username   :: T.Text
                , password   :: T.Text
                , baseUrl    :: String
                , manager    :: Manager
                , project    :: T.Text
                , debug      :: Bool }

runTCMBuild :: TCMBuild IO a -> BuildConfig -> IO a
runTCMBuild tcm config = evalStateT (runReaderT tcm config) Nothing

freshBuildConfig :: FileConfig -> T.Text -> Bool -> IO BuildConfig
freshBuildConfig FileConfig{..} project oDebug = do
    manager <- newManager tlsManagerSettings
    return $ BuildConfig user pass tcUrl manager project oDebug

tcBuild :: BuildConfig -> IO ()
tcBuild = runTCMBuild tcBuildM

tcBuildM :: TCMBuild IO ()
tcBuildM = do
    projects <- liftDL TC.listProjects
    searchProject <- asks project
    let project'  = find (\p -> (TC.name (p :: TC.Project)) == searchProject) $ TC.project projects
    when (isNothing project') $ quitError "This project does not exist"
    let project  = fromJust project'

    dbg "REST: Project Info"
    pInfoRes <- liftDL $ TC.restGetJson' (T.unpack $ TC.href (project :: TC.Project)) :: TCMBuild IO TC.ProjectInfo
    let buildTypes  = TC.buildType . TC.buildTypes $ pInfoRes
    when (length buildTypes == 0) $ quitError "This project has no build configuration"
    when (length buildTypes >= 2) $ quitError "This project has more than one build configuration"
    let buildTypeId = TC.id (head buildTypes :: TC.BuildType)

    let body = buildQueueBody buildTypeId
    TC.BuildQueueResponse{..} <- liftDL $ TC.restPostJson "buildQueue" body

    tcUrl <- asks baseUrl
    liftIO $ case state of
        "queued" -> do
            putStrLn $ "Build #" ++ show id ++ " has been queued: " ++ tcUrl ++ "viewLog.html?buildId=" ++ show id
        state -> do
            putStrLn $ "Build #" ++ show id ++ " in unknown state '" ++ T.unpack state ++ "': " ++ tcUrl ++ "viewLog.html?buildId=" ++ show id

buildQueueBody :: T.Text -> RequestBody
buildQueueBody id = RequestBodyBS . T.encodeUtf8 $
    "{\"buildType\":{\"id\":\"" <> id <> "\"}}"

liftDL :: MonadIO m => TC.TCMRest m a -> TCMBuild m a
liftDL = withReaderT (\BuildConfig{..} -> TC.RestConfig{..})

dbg :: MonadIO m => String -> TCMBuild m ()
dbg = liftDL . TC.dbg
