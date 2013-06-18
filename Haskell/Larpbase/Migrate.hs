{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Binary as Binary
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
            liftIO $ createSeries database newID
            XML.tagNoAttr "names" $ XML.many $ do
              XML.tagNoAttr "name" $ do
                name <- XML.content
                liftIO $ createSeriesName database newID name
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
            liftIO $ createCharacter database newSeriesID newID
            XML.tagNoAttr "names" $ XML.many $ do
              XML.tagNoAttr "name" $ do
                name <- XML.content
                liftIO $ createCharacterName database newID name
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


createSeries :: SQL.Database -> UUID.UUID -> IO ()
createSeries database seriesID = do
  statement <- SQL.prepare database
    "INSERT INTO series (series_id) VALUES (?)"
  SQL.bindBlob statement 1 (BS.concat $ LBS.toChunks $ Binary.encode seriesID)
  let loop = do
        result <- SQL.step statement
        case result of
          SQL.Row -> loop
          SQL.Done -> return ()
  loop
  SQL.finalize statement


createCharacter :: SQL.Database -> UUID.UUID -> UUID.UUID -> IO ()
createCharacter database seriesID characterID = do
  statement <- SQL.prepare database
    "INSERT INTO characters (series_id, character_id) VALUES (?, ?)"
  SQL.bindBlob statement 1
    (BS.concat $ LBS.toChunks $ Binary.encode seriesID)
  SQL.bindBlob statement 2
    (BS.concat $ LBS.toChunks $ Binary.encode characterID)
  let loop = do
        result <- SQL.step statement
        case result of
          SQL.Row -> loop
          SQL.Done -> return ()
  loop
  SQL.finalize statement


createSeriesName :: SQL.Database -> UUID.UUID -> Text.Text -> IO ()
createSeriesName database seriesID name = do
  statement <- SQL.prepare database $ Text.intercalate " "
    ["INSERT INTO series_names (series_id, rank, name) VALUES",
     "(?1, coalesce((SELECT max(rank) + 1 FROM series_names",
     "WHERE series_id = ?1), 0), ?2)"]
  SQL.bindBlob statement 1 (BS.concat $ LBS.toChunks $ Binary.encode seriesID)
  SQL.bindBlob statement 2 (Text.encodeUtf8 name)
  let loop = do
        result <- SQL.step statement
        case result of
          SQL.Row -> loop
          SQL.Done -> return ()
  loop
  SQL.finalize statement


createCharacterName :: SQL.Database -> UUID.UUID -> Text.Text -> IO ()
createCharacterName database characterID name = do
  statement <- SQL.prepare database $ Text.intercalate " "
    ["INSERT INTO character_names (character_id, rank, name) VALUES",
     "(?1, coalesce((SELECT max(rank) + 1 FROM character_names",
     "WHERE character_id = ?1), 0), ?2)"]
  SQL.bindBlob statement 1
    (BS.concat $ LBS.toChunks $ Binary.encode characterID)
  SQL.bindBlob statement 2 (Text.encodeUtf8 name)
  let loop = do
        result <- SQL.step statement
        case result of
          SQL.Row -> loop
          SQL.Done -> return ()
  loop
  SQL.finalize statement
