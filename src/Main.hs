{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.Aeson (decode, encode, object, toJSON, (.=))
import Data.Aeson.Types
import Data.ByteString.Char8 as BS8 (unpack)
import Data.ByteString as BS (ByteString)
import Data.CaseInsensitive (CI, original)
import Data.Text.Lazy
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (Port)
import System.Log.FastLogger
import Web.Scotty.Trans

import SystemInfo

main :: IO ()
main = do
  putStrLn "Starting..."
  logger <- newStdoutLoggerSet defaultBufSize
  chell logger 3000 $ do
    middleware $ requestLogger logger
    middleware $ responseLogger logger
    get "/status" $ do
      json $ toJSON mySI
    post "/status" $ do
      urls <- decode <$> body
      liftIO $ print urls
      case urls of
        Nothing -> json $ toJSON [mySI]
        Just urls' -> do
          sis <- liftIO $ mapM getSystemInfo urls'
          json $ toJSON $ (Right mySI) : sis
    notFound $ json $ object [ "error" .= ("not found" :: String) ]

type ChellM c = ScottyT Text (ReaderT c IO)
type ChellActionM c = ActionT Text (ReaderT c IO)

chell :: c -> Port -> ChellM c () -> IO ()
chell cfg port a = scottyT port (flip runReaderT cfg) a

requestLogger :: LoggerSet -> Middleware
requestLogger l app req sendResponse = do
  pushLogStrLn l $ reqToLogStr req
  app req $ \ res -> sendResponse res

responseLogger :: LoggerSet -> Middleware
responseLogger l app req sendResponse =
  app req $ \ res -> do
    pushLogStrLn l $ resToLogStr res
    sendResponse res

reqToLogStr :: Request -> LogStr
reqToLogStr r = toLogStr $ encode $ object [ "lvl" .= ("debug (replace with type!!!)" :: String)
                                           , "type" .= ("response" :: String)
                                           , "@timestamp" .= ("TBD" :: String)
                                           , "headers" .= (toJSON $ requestHeaders r)]

resToLogStr :: Response -> LogStr
resToLogStr r = toLogStr $ encode $ object [ "lvl" .= ("debug (replace with type!!!)" :: String)
                                           , "type" .= ("response" :: String)
                                           , "@timestamp" .= ("TBD" :: String)
                                           , "status" .= (toJSON $ responseStatus r)
                                           , "headers" .= (toJSON $ responseHeaders r)]

instance ToJSON Status where
  toJSON Status{..} = object [ "code" .= (toJSON statusCode)
                             , "message" .= BS8.unpack statusMessage]

instance ToJSON ByteString where
  toJSON = toJSON . BS8.unpack

instance ToJSON c => ToJSON (CI c) where
  toJSON = toJSON . original
