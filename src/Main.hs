{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode, object, toJSON, (.=))
import Data.Default (def)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.RequestLogger.JSON (formatAsJSON)
import Web.Scotty

import SystemInfo

main :: IO ()
main = do
  putStrLn "Starting..."
  logStdoutJSON <- mkRequestLogger (def { outputFormat = CustomOutputFormatWithDetails formatAsJSON})
  scotty 3000 $ do
    middleware logStdoutJSON
    get "/status" $ do
      json $ toJSON mySI
    post "/status" $ do
      urls <- decode <$> body
      liftIO $ print urls
      case urls of
        Nothing -> json $ toJSON [mySI]
        Just urls' -> do
          sis <- liftIO $ mapM getSystemInfo urls'
          json $ toJSON $ (Just mySI) : sis
    notFound $ json $ object [ "error" .= ("not found" :: String) ]
