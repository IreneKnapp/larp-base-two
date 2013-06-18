{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Aeson as JSON
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Database.SQLite3 as SQL
import qualified Network.HTTP.Types as HTTP
import qualified Network.Info as Network
import qualified Network.Socket as Network
import qualified Network.Wai as HTTP
import qualified Network.Wai.Handler.Warp as HTTP
import qualified System.Environment as System

import Control.Applicative
import Control.Concurrent
import Control.Monad.State
import Data.Int
import Data.String


data Configuration =
  Configuration {
      configurationDatabaseFilename :: FilePath,
      configurationLogFilename :: FilePath,
      configurationUserName :: Maybe String,
      configurationGroupName :: Maybe String,
      configurationPort :: Int
    }
instance JSON.FromJSON Configuration where
  parseJSON (JSON.Object v) =
    Configuration <$> v JSON..: "database"
                  <*> v JSON..: "log"
                  <*> v JSON..:? "user"
                  <*> v JSON..:? "group"
                  <*> v JSON..: "port"
  parseJSON _ = mzero


data ServerState =
  ServerState {
      serverStateDatabase :: MVar SQL.Database,
      serverStateCaptchaCache :: MVar (Map.Map Int64 (String, BS.ByteString)),
      serverStateSessionID :: Maybe Int64
    }


-- type Server = StateT ServerState HTTP.HTTP


main :: IO ()
main = do
  arguments <- System.getArgs
  case arguments of
    [configurationFilename] -> runService configurationFilename
    _ -> help


help :: IO ()
help = do
  putStrLn "Usage: larpbase configuration.json"


runService :: FilePath -> IO ()
runService configurationFilename = do
  configurationText <- Text.readFile configurationFilename
  case JSON.eitherDecode'
        $ LBS.fromChunks [Text.encodeUtf8 configurationText] of
    Left message -> do
      putStrLn $ "Invalid configuration: " ++ message
    Right configuration -> do
      database <-
        SQL.open $ Text.pack $ configurationDatabaseFilename configuration
      databaseMVar <- newMVar database
      captchaCacheMVar <- newMVar Map.empty
      let state = ServerState {
              serverStateDatabase = databaseMVar,
              serverStateCaptchaCache = captchaCacheMVar,
              serverStateSessionID = Nothing
            }
          port = configurationPort configuration
      HTTP.run port application


application :: HTTP.Application
application _ = do
  return $ HTTP.responseLBS HTTP.status200 [("Content-Type", "text/plain")]
                            "Hello, web!"
{-
accept :: Server ()
accept = do
  uri <- lift $ HTTP.getRequestURI
  case uri of
    "/" -> do
      lift $ HTTP.setResponseHeader HTTP.HttpContentType
        "text/html; charset=utf-8"
      lift $ HTTP.httpPutStr "Foo."
    "/api" -> do
      lift $ HTTP.setResponseHeader HTTP.HttpContentType
        "text/html; charset=utf-8"
      lift $ HTTP.httpPutStr "Bar."
    _ -> do
      lift $ HTTP.setResponseHeader HTTP.HttpContentType
        "text/html; charset=utf-8"
      lift $ HTTP.setResponseStatus 404
-}
