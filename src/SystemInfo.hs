{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}
module SystemInfo where

import Control.Applicative
import Control.Exception
import Data.Aeson
import Data.ByteString.Lazy.Char8 (unpack)
import Data.UUID
import Network.HTTP.Client

import BuildEnv

data SystemInfo = SI { siSystem::String
                     , siBuildNumber::String
                     , siBranch::String
                     , siCommit::String
                     , siSrvId::UUID
                     } deriving (Eq, Show)

instance FromJSON SystemInfo where
  parseJSON = withObject "SystemInfo" $
    \ o -> SI <$> o .: "system"
           <*> o .: "build-id"
           <*> o .: "branch"
           <*> o .: "commit"
           <*> o .: "srv-id"

instance ToJSON SystemInfo where
  toJSON si = object [ "system" .= siSystem si
                     , "build-id" .= siBuildNumber si
                     , "branch" .= siBranch si
                     , "commit" .= siCommit si
                     , "srv-id" .= siSrvId si
                     ]

mySI srvId = SI "check-status"
  ($(getBuildEnv "no-build-number" "CIRCLE_BUILD_NUM"))
  ($(getBuildEnv "no-branch" "CIRCLE_BRANCH"))
  ($(getBuildEnv "no-commit" "CIRCLE_SHA1"))
  srvId

getSystemInfo :: String -> IO (Either String SystemInfo)
getSystemInfo url = do
  mgr <- newManager defaultManagerSettings
  req <- parseRequest url
  handle (\ (e :: SomeException) -> return $ Left $ show e) $ do
    res <- httpLbs req mgr
    let res' = decode $ responseBody res
    case res' of
      Nothing -> return $ Left $ "Decoding failed: " ++ (unpack $ responseBody res)
      Just si -> return $ Right si
