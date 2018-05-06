module OptParse where

import Options.Applicative
import Data.Semigroup ((<>))


data Action
    = Download DownloadInfo
    | List
    | Build String
  deriving (Show)

data DownloadInfo =
    DownloadInfo { oProject :: String
                 , oBuild   :: Maybe String
                 } deriving (Show)

data Options =
    Options { oConfig  :: Maybe FilePath
            , oDebug   :: Bool
            , oAction  :: Action
            } deriving (Show)


parseCLI :: Completer -> IO Options
parseCLI projectCompleter = execParser (withInfo (parseOptions projectCompleter) "tccontrol")
    where withInfo opts h = info (helper <*> opts) $ header h

parseOptions :: Completer -> Parser Options
parseOptions projectCompleter = Options
    <$> (optional . strOption)
        ( long "config"
       <> short 'c'
       <> metavar "PATH"
       <> help "Path to config yml file" )
    <*> switch
        ( long "verbose"
       <> short 'v'
       <> help "Verbose mode" )
    <*> parseAction projectCompleter

parseAction :: Completer -> Parser Action
parseAction projectCompleter = hsubparser
    ( command "list" listInfo
   <> command "ls"   listInfo
   <> command "download" downloadInfo
   <> command "dl"       downloadInfo
   <> command "build" runInfo
    )
  where listInfo     = info (pure List)
                            (progDesc "List all projects")
        downloadInfo = info (Download <$> parseDownloadArgs projectCompleter)
                            (progDesc "Download project")
        runInfo      = info (Build <$> parseRunArgs projectCompleter)
                            (progDesc "Build project")

parseDownloadArgs :: Completer -> Parser DownloadInfo
parseDownloadArgs projectCompleter = DownloadInfo
    <$> argument str
        ( metavar "PROJECT"
       <> completer projectCompleter
       <> help "Name of project" )
    <*> optional (argument str
        ( metavar "BUILD"
       <> help "Build number" ))

parseRunArgs :: Completer -> Parser String
parseRunArgs projectCompleter = argument str
        ( metavar "PROJECT"
       <> completer projectCompleter
       <> help "Name of project" )
