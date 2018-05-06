{-# LANGUAGE DuplicateRecordFields #-}

module TCDownload where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.List
import Data.Maybe
import Data.Ord
import Data.Semigroup ((<>))
import Data.Time.LocalTime

import Network.HTTP.Conduit
import Network.HTTP.Types.Header

import System.FilePath ((</>))

import FileConfig
import qualified TCRest as TC
import Util

type TCMDL m = TC.TCM DownloadConfig m

data DownloadConfig =
    DownloadConfig { username   :: T.Text
                   , password   :: T.Text
                   , baseUrl    :: String
                   , manager    :: Manager
                   , project    :: T.Text
                   , build      :: Maybe T.Text
                   , debug      :: Bool
                   , targetDir' :: FilePath
                   , excepts'   :: M.Map T.Text FilePath }

runTCMDL :: TCMDL IO a -> DownloadConfig -> IO a
runTCMDL tcm config = evalStateT (runReaderT tcm config) Nothing

freshConfig :: FileConfig -> T.Text -> Maybe T.Text -> Bool -> IO DownloadConfig
freshConfig FileConfig{..} project build oDebug = do
    manager <- newManager tlsManagerSettings
    return $ DownloadConfig user pass tcUrl manager project build oDebug targetDir excepts

tcDownload :: DownloadConfig -> IO ()
tcDownload = runTCMDL tcDownloadM

tcDownloadM :: TCMDL IO ()
tcDownloadM = do
    dbg "REST: List Projects"
    projects      <- liftDL TC.listProjects
    searchProject <- asks project
    let project   = find (\p -> (TC.name (p :: TC.Project)) == searchProject) $ TC.project projects
    when (isNothing project) $ quitError "This project does not exist"
    let project'  = fromJust project

    dbg "REST: Project Info"
    pInfoRes       <- liftDL $ TC.restGetJson' (T.unpack $ TC.href (project' :: TC.Project)) :: TCMDL IO TC.ProjectInfo
    let buildTypes  = TC.buildType . TC.buildTypes $ pInfoRes
    when (length buildTypes == 0) $ quitError "This project has no build configuration"
    when (length buildTypes >= 2) $ quitError "This project has more than one build configuration"
    let buildTypeId = TC.id (head buildTypes :: TC.BuildType)

    let buildsUrl   = "buildTypes/id:" ++ T.unpack buildTypeId ++ "/builds"
    buildsRes      <- liftDL $ TC.restGetJson buildsUrl :: TCMDL IO TC.Builds
    build          <- buildSelector $ TC.build (buildsRes :: TC.Builds)
    let buildStatus = T.unpack $ TC.status build
    when (buildStatus /= "SUCCESS") $ quitError $ "This build was not successful" ++ buildStatus
    let buildNumber = TC.number build

    let buildInfoUrl = T.unpack $ TC.href (build :: TC.Build)
    buildInfoRes    <- liftDL $ TC.restGetJson' buildInfoUrl :: TCMDL IO TC.BuildInfo
    buildTime       <- liftIO.utcToLocalZonedTime $ TC.finishDate buildInfoRes

    let artifactUrl = buildInfoUrl ++ "/artifacts"
    artifacts      <- liftDL $ TC.getArtifacts artifactUrl

    targetDir <- getTargetDir searchProject
    forM artifacts $ uncurry (downloadArtifact artifactUrl targetDir)
    dbg $ show artifacts
    liftIO . T.putStrLn $ "Downloaded "
                       <> searchProject
                       <> " #"
                       <> buildNumber
                       <> " built at "
                       <> tShow buildTime

downloadArtifact :: String -> FilePath -> T.Text -> T.Text -> TCMDL IO ()
downloadArtifact restPrefix targetDir filename path = do
    let dlPath = restPrefix ++ "/content/" ++ T.unpack path
    dbg $ "Downloading " ++ dlPath
    res <- liftDL $ TC.restGet' dlPath [(hAccept, "*/*")]

    let targetFile = targetDir </> T.unpack filename
    dbg $ "Writing " ++ T.unpack filename ++ " to " ++ targetFile
    liftIO $ BSL.writeFile targetFile $ responseBody res

getTargetDir :: Monad m => T.Text -> TCMDL m FilePath
getTargetDir filename = do
    DownloadConfig{..} <- ask
    return $ M.findWithDefault targetDir' filename excepts'

buildSelector :: MonadIO m => [TC.Build] -> TCMDL m TC.Build
buildSelector builds = do
    buildNr <- asks build
    case buildNr of
        Nothing -> do
            let successBuilds = filter ((== "SUCCESS") . TC.status) builds
            return $ maximumBy (comparing (TC.id :: TC.Build -> Int)) successBuilds
        Just nr -> do
            let b = find ((== nr) . (TC.number :: TC.Build -> T.Text)) builds
            when (isNothing b) $ quitError "This build number does not exist"
            return $ fromJust b

liftDL :: MonadIO m => TC.TCMRest m a -> TCMDL m a
liftDL = withReaderT (\DownloadConfig{..} -> TC.RestConfig{..})

dbg :: MonadIO m => String -> TCMDL m ()
dbg = liftDL . TC.dbg
