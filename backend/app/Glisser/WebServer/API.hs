{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Glisser.WebServer.API (
    runWebServer,
) where

import Web.Spock as Spock
import Web.Spock.Config as Spock
import Network.HTTP.Types.Status (status200)

import Data.Aeson as A
import Data.Text (Text)
import GHC.Generics (Generic)

type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

newtype Move = Move
  { moveContent :: Text
  } deriving (Generic, Show)

instance ToJSON Move
instance FromJSON Move

newtype Chat = Chat
  { chatContent :: Text
  } deriving (Generic, Show)

instance ToJSON Chat
instance FromJSON Chat

runWebServer :: IO ()
runWebServer = do
    config <- defaultSpockCfg () PCNoDatabase ()
    runSpock 8080 $ spock config $ do
        get root $ do
            Spock.html "<div>Welcome to the Glisser API</div>"
        post ("game" <//> var <//> "move") $ \(gameid :: Int) -> do
            move <- jsonBody' :: ApiAction Move
            Spock.setStatus status200
        post ("game" <//> var <//> "chat") $ \(gameid :: Int) -> do
            chat <- jsonBody' :: ApiAction Chat
            -- Return 200: OK
            Spock.setStatus status200
  where users :: [String]
        users = ["bob", "alice"]
