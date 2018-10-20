{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.Aeson (decode, encode, object, toJSON, (.=))
import Data.Aeson.Types
import Data.ByteString as BS (ByteString)
import Data.ByteString.Char8 as BS8 (unpack)
import Data.CaseInsensitive (CI, original)
import Data.Text.Lazy
import Data.Time
import Data.UUID
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (Port)
import System.Log.FastLogger
import System.Random
import Web.Scotty.Trans

import SystemInfo

main :: IO ()
main = do
  putStrLn "Starting..."
  logger <- newStdoutLoggerSet defaultBufSize
  srvId <- randomIO :: IO UUID
  chell logger 3000 $ do
    middleware $ requestLogger logger
    middleware $ responseLogger logger
    get "/status" $ json $ toJSON $ mySI srvId
    post "/status" $ do
      urls <- decode <$> body
      liftIO $ print urls
      case urls of
        Nothing -> json $ toJSON [mySI srvId]
        Just urls' -> do
          sis <- liftIO $ mapM getSystemInfo urls'
          json $ toJSON $ (Right $ mySI srvId) : sis
    notFound $ json $ object [ "error" .= ("not found" :: String) ]

type ChellM c = ScottyT Text (ReaderT c IO)
type ChellActionM c = ActionT Text (ReaderT c IO)

chell :: c -> Port -> ChellM c () -> IO ()
chell cfg port = scottyT port (`runReaderT` cfg)

requestLogger :: LoggerSet -> Middleware
requestLogger l app req sendResponse = do
  reqToLogStr req >>= pushLogStrLn l
  app req $ \ res -> sendResponse res

responseLogger :: LoggerSet -> Middleware
responseLogger l app req sendResponse =
  app req $ \ res -> do
  resToLogStr res >>= pushLogStrLn l
  sendResponse res

reqToLogStr :: Request -> IO LogStr
reqToLogStr r = do
  time <- getCurrentTime
  let format = iso8601DateFormat (Just "%H:%M:%S%EZ")
  return $ toLogStr $ encode $ object [ "lvl" .= ("debug" :: String)
                                      , "type" .= ("request" :: String)
                                      , "timestamp" .= formatTime defaultTimeLocale format time
                                      , "headers" .= toJSON (requestHeaders r)]

resToLogStr :: Response -> IO LogStr
resToLogStr r = do
  time <- getCurrentTime
  let format = iso8601DateFormat (Just "%H:%M:%S%EZ")
  return $ toLogStr $ encode $ object [ "lvl" .= ("debug" :: String)
                                      , "type" .= ("response" :: String)
                                      , "timestamp" .= formatTime defaultTimeLocale format time
                                      , "status" .= toJSON (responseStatus r)
                                      , "headers" .= toJSON (responseHeaders r)]

instance ToJSON Status where
  toJSON Status{..} = object [ "code" .= toJSON statusCode
                             , "message" .= BS8.unpack statusMessage]

instance ToJSON ByteString where
  toJSON = toJSON . BS8.unpack

instance ToJSON c => ToJSON (CI c) where
  toJSON = toJSON . original
