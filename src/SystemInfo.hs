{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}
module SystemInfo where

import Control.Applicative
import Control.Exception
import Data.Aeson
import Network.HTTP.Client

import BuildEnv

data SystemInfo = SI { siSystem::String
                     , siBuildNumber::String
                     , siBranch::String
                     , siCommit::String
                     } deriving (Eq, Show)

instance FromJSON SystemInfo where
  parseJSON = withObject "SystemInfo" $
    \ o -> SI <$> o .: "system"
           <*> o .: "build-id"
           <*> o .: "branch"
           <*> o .: "commit"

instance ToJSON SystemInfo where
  toJSON si = object [ "system" .= siSystem si
                     , "build-id" .= siBuildNumber si
                     , "branch" .= siBranch si
                     , "commit" .= siCommit si
                     ]

mySI = SI "check-status"
       ($(getBuildEnv "no-build-number" "CIRCLE_BUILD_NUM"))
       ($(getBuildEnv "no-branch" "CIRCLE_BRANCH"))
       ($(getBuildEnv "no-commit" "CIRCLE_SHA1"))

getSystemInfo :: String -> IO (Maybe SystemInfo)
getSystemInfo url = do
  mgr <- newManager defaultManagerSettings
  req <- parseRequest url
  handle (\ (_ :: SomeException) -> return Nothing) $ do
    res <- httpLbs req mgr
    return (decode $ responseBody res)
