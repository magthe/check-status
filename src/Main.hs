{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson (object, toJSON, (.=))
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
      json $ toJSON (Just [mySI])
    notFound $ json $ object [ "error" .= ("not found" :: String) ]
