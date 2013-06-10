{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.UUID as UUID
import qualified Database.SQLite3 as SQL
import qualified Filesystem.Path.CurrentOS as System
import qualified Network.HTTP as HTTP
import qualified Network.Info as Network
import qualified Network.Socket as Network
import qualified System.Directory as System
import qualified System.Environment as System
import qualified System.Exit as System
import qualified System.UUID.V1 as UUID
import qualified Text.XML.Stream.Parse as XML

import Control.Applicative
import Control.Concurrent
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Int
import Data.String


main :: IO ()
main = do
  arguments <- System.getArgs
  case arguments of
    [databaseFileName, charactersFileName] -> do
      database <- initializeDatabase databaseFileName
      migrateCharacters database charactersFileName
    _ -> do
      putStrLn "Usage: larpbase-migrate database.db characters.xml"
      System.exitFailure


initializeDatabase :: FilePath -> IO SQL.Database
initializeDatabase fileName = do
  exists <- System.doesFileExist fileName
  if exists
    then do
      putStrLn "Database must not already exist."
      System.exitFailure
    else do
      database <- SQL.open (Text.pack fileName)
      SQL.exec database $ Text.intercalate "\n"
        ["CREATE TABLE series (",
         "  series_id BLOB PRIMARY KEY",
         ");"]
      SQL.exec database $ Text.intercalate "\n"
        ["CREATE TABLE series_names (",
         "    series_id BLOB,",
         "    rank INT,",
         "    name TEXT,",
         "    PRIMARY KEY (series_id, rank),",
         "    UNIQUE (series_id, name)",
         ");"]
      SQL.exec database $ Text.intercalate "\n"
        ["CREATE TABLE characters (",
         "    character_id BLOB PRIMARY KEY,",
         "    series_id BLOB",
         ");"]
      SQL.exec database $ Text.intercalate "\n"
        ["CREATE TABLE character_names (",
         "    character_id BLOB,",
         "    rank INT,",
         "    name TEXT,",
         "    PRIMARY KEY (character_id, rank),",
         "    UNIQUE (character_id, name)",
         ");"]
      return database


migrateCharacters :: SQL.Database -> FilePath -> IO ()
migrateCharacters database fileName = do
  runResourceT $ do
    seriesIDs <- liftIO $ newMVar Map.empty
    characterIDs <- liftIO $ newMVar Map.empty
    XML.parseFile XML.def (fromString fileName) $$ do
      XML.tagNoAttr "nekochars" $ do
        XML.tagNoAttr "all-series" $ XML.many $ do
          XML.tagName "series" (XML.requireAttr "id") $ \oldID -> do
            newID <- liftIO $ migrateID seriesIDs oldID
            liftIO $ putStrLn $ ""
            liftIO $ putStrLn $ "old series id: " ++ Text.unpack oldID
            liftIO $ putStrLn $ "new series id: " ++ show newID
            XML.tagNoAttr "names" $ XML.many $ do
              XML.tagNoAttr "name" $ do
                name <- XML.content
                liftIO $ putStrLn $ "name: " ++ Text.unpack name
                return ()
            return ()
        XML.tagNoAttr "all-characters" $ XML.many $ do
          XML.tagName "character"
              (do
                id <- XML.requireAttr "id"
                series <- XML.requireAttr "series"
                return (id, series))
              $ \(oldID, oldSeriesID) -> do
            newID <- liftIO $ migrateID characterIDs oldID
            newSeriesID <- liftIO $ migrateID seriesIDs oldSeriesID
            liftIO $ putStrLn $ ""
            liftIO $ putStrLn $ "old character id: " ++ Text.unpack oldID
            liftIO $ putStrLn $ "new character id: " ++ show newID
            liftIO $ putStrLn $ "old series id: " ++ Text.unpack oldSeriesID
            liftIO $ putStrLn $ "new series id: " ++ show newSeriesID
            XML.tagNoAttr "names" $ XML.many $ do
              XML.tagNoAttr "name" $ do
                name <- XML.content
                liftIO $ putStrLn $ "name: " ++ Text.unpack name
                return ()
            return ()
      return ()


migrateID :: MVar (Map.Map Text.Text UUID.UUID) -> Text.Text -> IO UUID.UUID
migrateID allIDsMVar textID = do
  allIDs <- takeMVar allIDsMVar
  case Map.lookup textID allIDs of
    Just uuid -> do
      putMVar allIDsMVar allIDs
      return uuid
    Nothing -> do
      uuid <- case reads $ Text.unpack textID of
                [(uuid, "")] -> return uuid
                _ -> UUID.uuid
      let allIDs' = Map.insert textID uuid allIDs
      putMVar allIDsMVar allIDs'
      return uuid
