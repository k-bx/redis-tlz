{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Common.Redis
import           Control.Monad
import qualified Data.Aeson            as J
import qualified Data.ByteString.Char8 as B8
import           Data.ByteString.Lazy  (writeFile, readFile)
import qualified Database.Redis        as R
import           Options.Applicative
import           Prelude               hiding (writeFile, readFile)
import           Types

data Options =
    ImportOptions
      { _ioptHost        :: String
      , _ioptPort        :: Int
      , _ioptKeysPattern :: String
      , _ioptOutFile     :: String }
  | ExportOptions
      { _eoptHost        :: String
      , _eoptPort        :: Int
      , _eoptInFile     :: String }
  deriving (Show)

main :: IO ()
main = execParser opts >>= run
  where
    opts =
      info
        (helper <*>
         (subparser
            ((command "import"
                      (info (helper <*> parseImportOptions)
                            (fullDesc <> progDesc "Redis import utility")))
             <>
             (command "export"
                      (info (helper <*> parseExportOptions)
                            (fullDesc <> progDesc "Redis import utility"))))))
           (fullDesc <> progDesc "Redis import/export utility")

run :: Options -> IO ()
run opts = do
  case opts of
    ImportOptions host port pattern outfile -> do
      putStrLn $ "Starting import for pattern " ++ show pattern
      redis <- connectToRedis host port
      keys <- runRedisE redis $ R.keys (B8.pack pattern)
      putStrLn $ show (length keys) ++ " keys found"
      kvs <- forM keys $ \key -> do
          t <- runRedisE redis $ R.getType key
          RedisKV <$> pure key <*> getRedisValue redis t key
      let dump = Dump kvs
      let outfileFull = outfile
      writeFile outfileFull (J.encode dump)
      return ()
    ExportOptions host port infile -> do
      putStrLn $ "Starting export from file " ++ infile
      redis <- connectToRedis host port
      edump <- J.eitherDecode <$> (readFile infile)
      case edump of
        Left err -> putStrLn $ "There was an error decoding dump. " ++ err
        Right dump -> do
          forM_ (_dItems dump) $ do
            storeRedisValue redis
          putStrLn $ "Exported " ++ show (length (_dItems dump)) ++ " items."
  where
    connectToRedis host port = R.connect (R.defaultConnectInfo
                                 { R.connectHost = host
                                 , R.connectPort =
                                     R.PortNumber (fromIntegral (port)) })

parseImportOptions :: Parser Options
parseImportOptions =
  ImportOptions
    <$> strOption (long "host"
                   <> value "localhost"
                   <> help "Redis host")
    <*> option (long "port"
                <> value 6379
                <> help "Redis port")
    <*> strOption (long "keys"
                   <> help ("Pattern to select from redis "
                         ++ "(use \"*\" for all keys)"))
    <*> strOption (long "outfile"
                   <> value "redis-dump.json"
                   <> help "Output file with dump")


parseExportOptions :: Parser Options
parseExportOptions =
  ExportOptions
    <$> strOption (long "host"
                   <> value "localhost"
                   <> help "Redis host")
    <*> option (long "port"
                <> value 6379
                <> help "Redis port")
    <*> strOption (long "infile"
                   <> value "redis-dump.json"
                   <> help "Input file with dump")
