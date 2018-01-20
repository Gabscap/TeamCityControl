{-# LANGUAGE DuplicateRecordFields #-}

module TCDownloader where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import Control.Monad.Catch
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Aeson as Json
import Data.List
import Data.Maybe
import Data.Ord
import Data.Semigroup ((<>))
import Data.Time.LocalTime

import Network.HTTP.Conduit
import Network.HTTP.Types.Header

import System.FilePath ((</>))
import System.IO

import FileConfig
import qualified TCResponse as TC
import Util

teamcityRestPath :: String
teamcityRestPath = "/app/rest/"

data Config = Config { username   :: T.Text
                     , password   :: T.Text
                     , baseUrl    :: String
                     , manager    :: Manager
                     , project    :: T.Text
                     , build      :: Maybe T.Text
                     , debug      :: Bool
                     , targetDir' :: FilePath
                     , excepts'   :: M.Map T.Text FilePath }

type TCM m = StateT (Maybe CookieJar) (ReaderT Config m)

runTCM :: TCM IO a -> Config -> IO a
runTCM tcm = runReaderT (evalStateT tcm Nothing)

freshConfig :: FileConfig -> T.Text -> Maybe T.Text -> Bool -> IO Config
freshConfig FileConfig{..} project build oDebug = do
    manager <- newManager tlsManagerSettings
    return $ Config user pass tcUrl manager project build oDebug targetDir excepts

mainProgram :: TCM IO ()
mainProgram = do
    dbg "REST: List Projects"
    projects      <- listProjects
    searchProject <- asks project
    let project   = find (\p -> (TC.name (p :: TC.Project)) == searchProject) $ TC.project projects
    when (isNothing project) $ quitError "This project does not exist"
    let project'  = fromJust project

    dbg "REST: Project Info"
    pInfoRes       <- restGetJson' (T.unpack $ TC.href (project' :: TC.Project)) :: TCM IO TC.ProjectInfo
    let buildTypes  = TC.buildType . TC.buildTypes $ pInfoRes
    when (length buildTypes == 0) $ quitError "This project has no build configuration"
    when (length buildTypes >= 2) $ quitError "This project has more than one build configuration"
    let buildTypeId = TC.id (head buildTypes :: TC.BuildType)

    let buildsUrl   = "buildTypes/id:" ++ T.unpack buildTypeId ++ "/builds"
    buildsRes      <- restGetJson buildsUrl :: TCM IO TC.Builds
    build          <- buildSelector $ TC.build (buildsRes :: TC.Builds)
    let buildStatus = T.unpack $ TC.status build
    when (buildStatus /= "SUCCESS") $ quitError $ "This build was not successful" ++ buildStatus
    let buildNumber = TC.number build

    let buildInfoUrl = T.unpack $ TC.href (build :: TC.Build)
    buildInfoRes    <- restGetJson' buildInfoUrl :: TCM IO TC.BuildInfo
    buildTime       <- liftIO.utcToLocalZonedTime $ TC.finishDate buildInfoRes

    let artifactUrl = buildInfoUrl ++ "/artifacts"
    artifacts      <- getArtifacts artifactUrl

    targetDir <- getTargetDir searchProject
    forM artifacts $ uncurry (downloadArtifact artifactUrl targetDir)
    dbg $ show artifacts
    liftIO . T.putStrLn $ "Downloaded "
                       <> searchProject
                       <> " #"
                       <> buildNumber
                       <> " built at "
                       <> tShow buildTime

downloadArtifact :: String -> FilePath -> T.Text -> T.Text -> TCM IO ()
downloadArtifact restPrefix targetDir filename path = do
    let dlPath = restPrefix ++ "/content/" ++ T.unpack path
    dbg $ "Downloading " ++ dlPath
    res <- restGet' dlPath [(hAccept, "*/*")]

    let targetFile = targetDir </> T.unpack filename
    dbg $ "Writing " ++ T.unpack filename ++ " to " ++ targetFile
    liftIO $ BSL.writeFile targetFile $ responseBody res

getTargetDir :: Monad m => T.Text -> TCM m FilePath
getTargetDir filename = do
    Config{..} <- ask
    return $ M.findWithDefault targetDir' filename excepts'

buildSelector :: MonadIO m => [TC.Build] -> TCM m TC.Build
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

getArtifacts :: String -> TCM IO [(T.Text, T.Text)]
getArtifacts path = do
    filesRes <- restGetJson' path :: TCM IO TC.Files
    let files = TC.file filesRes
    concatMapM fromFile files
    where fromFile TC.File{..} = do
              cs <- case children of
                Nothing          -> return []
                Just TC.Href{..} -> getArtifacts $ T.unpack href
              let cs' = fmap ((name <> "/") <>) <$> cs
              case content of
                Nothing          -> return cs'
                Just TC.Href{..} -> return $ (name, name):cs'

listProjectNames :: TCM IO [T.Text]
listProjectNames = do
    projectsRes <- listProjects
    let names = (TC.name :: TC.Project -> T.Text) <$> TC.project projectsRes
    return $ filter (not . T.isPrefixOf "_") names

listProjects :: TCM IO TC.Projects
listProjects = restGetJson "projects"

----------
-- REST --
----------

restGetJson :: (MonadIO m, MonadThrow m, FromJSON a) => String -> TCM m a
restGetJson path = restGetJson' $ teamcityRestPath ++ path
    
restGetJson' :: (MonadIO m, MonadThrow m, FromJSON a) => String -> TCM m a
restGetJson' path = do
    res    <- restGet' path [(hAccept, "application/json")]
    let bs  = responseBody res
    dbg $ "===== GET " ++ path ++ " =====\n" ++ T.unpack (T.decodeUtf8 $ BSL.toStrict bs)
    either (throwM.JsonDecodeException) return $ eitherDecode bs

restGet' :: (MonadIO m, MonadThrow m) => String -> RequestHeaders -> TCM m (Response BSL.ByteString)
restGet' path reqHeaders = do
    restUrl <- asks baseUrl
    req     <- parseRequest $ restUrl ++ path
    let req' = req{ requestHeaders = reqHeaders }
    req''   <- auth req'
    manager <- asks manager
    res     <- httpLbs req'' manager
    put . Just $ responseCookieJar res
    return res

auth :: MonadIO m => Request -> TCM m Request
auth request = do
    Config{..} <- ask
    let hs  = requestHeaders request
    let req = request{ requestHeaders = ("Origin", T.encodeUtf8 $ T.pack baseUrl):hs }
    get >>= \case
        Nothing -> do
            let user = T.encodeUtf8 username
            let pass = T.encodeUtf8 password
            return $ applyBasicAuth user pass req
        Just cookies -> return req{ cookieJar = Just cookies }

data JsonDecodeException = JsonDecodeException String
instance Show JsonDecodeException where
    show (JsonDecodeException msg) = "JsonDecodeException: " ++ msg
instance Exception JsonDecodeException

dbg :: MonadIO m => String -> TCM m ()
dbg msg = whenM (asks debug) $ liftIO $ hPutStrLn stderr msg
