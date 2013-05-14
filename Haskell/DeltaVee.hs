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
import qualified Network.HTTP as HTTP
import qualified Network.Info as Network
import qualified Network.Socket as Network
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
      configurationListeners :: [Listener]
    }
instance JSON.FromJSON Configuration where
  parseJSON (JSON.Object v) =
    Configuration <$> v JSON..: "database"
                  <*> v JSON..: "log"
                  <*> v JSON..:? "user"
                  <*> v JSON..:? "group"
                  <*> v JSON..: "listeners"
  parseJSON _ = mzero


data Listener =
  Listener {
      listenerInterface :: Maybe Network.HostAddress,
      listenerPort :: Network.PortNumber
    }
  deriving (Show)
instance JSON.FromJSON Listener where
  parseJSON (JSON.Object v) = do
    port <- v JSON..: "port" >>= \port -> return $ fromIntegral (port :: Int)
    Listener <$> v JSON..:? "interface"
             <*> pure port
  parseJSON _ = mzero


data ServerState =
  ServerState {
      serverStateDatabase :: MVar SQL.Database,
      serverStateCaptchaCache :: MVar (Map.Map Int64 (String, BS.ByteString)),
      serverStateSessionID :: Maybe Int64
    }


type Server = StateT ServerState HTTP.HTTP


main :: IO ()
main = do
  arguments <- System.getArgs
  case arguments of
    [configurationFilename] -> runService configurationFilename
    _ -> help


help :: IO ()
help = do
  putStrLn "Usage: deltavee configuration.json"


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
      allInterfaces <- getAllInterfaces
      let state = ServerState {
              serverStateDatabase = databaseMVar,
              serverStateCaptchaCache = captchaCacheMVar,
              serverStateSessionID = Nothing
            }
          serverParameters = HTTP.HTTPServerParameters {
              HTTP.serverParametersAccessLogPath = Nothing,
              HTTP.serverParametersErrorLogPath =
                Just $ configurationLogFilename configuration,
              HTTP.serverParametersDaemonize = True,
              HTTP.serverParametersUserToChangeTo =
                configurationUserName configuration,
              HTTP.serverParametersGroupToChangeTo =
                configurationGroupName configuration,
              HTTP.serverParametersForkPrimitive = forkIO,
              HTTP.serverParametersListenSockets =
                concatMap
                  (\listener ->
                     let portNumber = listenerPort listener
                         socketAddresses =
                           case listenerInterface listener of
                             Nothing ->
                               map (\interface ->
                                      Network.SockAddrInet portNumber
                                                           interface)
                                   allInterfaces
                             Just hostAddress ->
                               [Network.SockAddrInet portNumber hostAddress]
                     in map (\socketAddress ->
                               HTTP.HTTPListenSocketParameters {
                                   HTTP.listenSocketParametersAddress =
                                     socketAddress,
                                   HTTP.listenSocketParametersSecure = False
                                 })
                            socketAddresses)
                  (configurationListeners configuration)
            }
      HTTP.acceptLoop serverParameters $ evalStateT accept state


getAllInterfaces :: IO [Network.HostAddress]
getAllInterfaces = do
  interfaces <- Network.getNetworkInterfaces
  return $ map (\interface ->
                  let Network.IPv4 hostAddress = Network.ipv4 interface
                  in hostAddress)
               interfaces


accept :: Server ()
accept = do
  lift $ HTTP.httpLog "Hmm."

