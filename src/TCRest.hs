{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module TCRest where

import Prelude hiding (id)
import Data.Aeson
import Data.Time.Clock
import Data.Time.Format
import GHC.Generics

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Control.Monad.Catch
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Semigroup ((<>))

import Network.HTTP.Conduit
import Network.HTTP.Simple (setRequestBody)
import Network.HTTP.Types.Header

import System.IO

import FileConfig

teamcityRestPath :: String
teamcityRestPath = "/app/rest/"

type TCM c m = ReaderT c (StateT (Maybe CookieJar) m)
type TCMRest m = TCM RestConfig m

data RestConfig =
    RestConfig { username   :: T.Text
               , password   :: T.Text
               , baseUrl    :: String
               , manager    :: Manager
               , debug      :: Bool }

runTCMRest :: TCMRest IO a -> RestConfig -> IO a
runTCMRest tcm config = evalStateT (runReaderT tcm config) Nothing

freshConfig :: FileConfig -> Bool -> IO RestConfig
freshConfig FileConfig{..} oDebug = do
    manager <- newManager tlsManagerSettings
    return $ RestConfig user pass tcUrl manager oDebug

getArtifacts :: String -> TCMRest IO [(T.Text, T.Text)]
getArtifacts path = do
    filesRes <- restGetJson' path :: TCMRest IO Files
    let files = file filesRes
    concatMapM fromFile files
    where fromFile File{..} = do
              cs <- case children of
                Nothing          -> return []
                Just Href{..} -> getArtifacts $ T.unpack href
              let cs' = fmap ((name <> "/") <>) <$> cs
              case content of
                Nothing          -> return cs'
                Just Href{..} -> return $ (name, name):cs'

listProjectNames :: TCMRest IO [T.Text]
listProjectNames = do
    projectsRes <- listProjects
    let names = (id :: Project -> T.Text) <$> project projectsRes
    return $ filter (not . T.isPrefixOf "_") names

listProjects :: TCMRest IO Projects
listProjects = restGetJson "projects"

----------
-- REST --
----------

restGetJson :: (MonadIO m, MonadThrow m, FromJSON a) => String -> TCMRest m a
restGetJson path = restGetJson' $ teamcityRestPath ++ path
    
restGetJson' :: (MonadIO m, MonadThrow m, FromJSON a) => String -> TCMRest m a
restGetJson' path = do
    res    <- restGet' path [(hAccept, "application/json")]
    let bs  = responseBody res
    dbg $ "===== GET " ++ path ++ " =====\n" ++ T.unpack (T.decodeUtf8 $ BSL.toStrict bs)
    either (throwM.JsonDecodeException) return $ eitherDecode bs

restGet' :: (MonadIO m, MonadThrow m) => String -> RequestHeaders -> TCMRest m (Response BSL.ByteString)
restGet' path reqHeaders = do
    restUrl <- asks baseUrl
    req     <- parseRequest $ restUrl ++ path
    let req' = req{ requestHeaders = reqHeaders }
    req''   <- auth req'
    manager <- asks manager
    res     <- httpLbs req'' manager
    put . Just $ responseCookieJar res
    return res

restPostJson :: (MonadIO m, MonadThrow m, FromJSON a) => String -> RequestBody -> TCMRest m a
restPostJson path body = do
    RestConfig{..} <- ask
    res    <- restPost path [(hAccept, "application/json")
                            ,(hContentType, "application/json")] body
    let bs  = responseBody res
    dbg $ "===== POST " ++ path ++ " =====\n" ++ T.unpack (T.decodeUtf8 $ BSL.toStrict bs)
    either (throwM.JsonDecodeException) return $ eitherDecode bs

restPost :: (MonadIO m, MonadThrow m) => String -> RequestHeaders -> RequestBody -> TCMRest m (Response BSL.ByteString)
restPost path reqHeaders body = do
    restUrl <- asks baseUrl
    req     <- parseRequest $ restUrl ++ teamcityRestPath ++ path
    let req' = req{ requestHeaders = reqHeaders
                  , method = "POST" }
    req''   <- auth . setRequestBody body $ req'
    manager <- asks manager
    res     <- httpLbs req'' manager
    put . Just $ responseCookieJar res
    return res
    

auth :: MonadIO m => Request -> TCMRest m Request
auth request = do
    RestConfig{..} <- ask
    let hs  = requestHeaders request
    let req = request{ requestHeaders = ("Origin", T.encodeUtf8 $ T.pack baseUrl):hs }
    get >>= \case
        Nothing -> do
            let user = T.encodeUtf8 username
            let pass = T.encodeUtf8 password
            return $ applyBasicAuth user pass req
        Just cookies -> return req{ cookieJar = Just cookies }

newtype JsonDecodeException = JsonDecodeException String
instance Show JsonDecodeException where
    show (JsonDecodeException msg) = "JsonDecodeException: " ++ msg
instance Exception JsonDecodeException

dbg :: MonadIO m => String -> TCMRest m ()
dbg msg = whenM (asks debug) $ liftIO $ hPutStrLn stderr msg

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

newtype Href =
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

data BuildQueueResponse =
    BuildQueueResponse { id          :: Int
                       , buildTypeId :: T.Text
                       , state       :: T.Text }
    deriving (Show, Generic)
instance FromJSON BuildQueueResponse

readTCTime :: ParseTime t => String -> t
readTCTime = parseTimeOrError True defaultTimeLocale "%Y%m%dT%H%M%S%z"
