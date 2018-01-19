module OptParse where

import Options.Applicative
import Data.Semigroup ((<>))


data Options =
    Options { oConfig  :: Maybe FilePath
            , oDebug   :: Bool
            , oProject :: String
            , oBuild   :: Maybe String }
    deriving (Show)


parseCLI :: Completer -> IO Options
parseCLI projectCompleter = execParser (withInfo (parseOptions projectCompleter) "TeamCityDownloader")
    where withInfo opts h = info (helper <*> opts) $ header h

parseOptions :: Completer -> Parser Options
parseOptions projectCompleter = Options
    <$> optional (strOption
        ( long "config"
       <> short 'c'
       <> metavar "PATH"
       <> help "Path to config yml file" ))
    <*> switch
        ( long "debug"
       <> short 'd'
       <> help "Debug mode" )
    <*> argument str
        ( metavar "PROJECT"
       <> completer projectCompleter
       <> help "Name of project" )
    <*> optional (argument str
        ( metavar "BUILD"
       <> help "Build number" ))
