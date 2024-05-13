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
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.String (renderHtml)

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

spockHtml :: H.Html -> ApiAction ()
spockHtml = Spock.html . pack . renderHtml

runWebServer :: IO ()
runWebServer = do
    config <- defaultSpockCfg () PCNoDatabase ()
    runSpock 8080 $ spock config $ do
        get root $ do
            spockHtml $ H.div "Welcome to the Glisser API"
        -- Lichess has this as game/stream/var, should figure out why this is or if it's just inconsistent
        get ("game" <//> var <//> "stream") $ \(gameid :: Int) -> do
            -- Return the current game state, including the players playing
            -- https://lichess.org/api#tag/Bot/operation/botGameStream for reference
            -- Spock.setStatus status200
            Spock.json $ A.object ["gameid" .= gameid]

        get ("game" <//> var <//> "chat") $ \(gameid :: Int) -> do
            -- Return the chat messages for the game
            Spock.setStatus status200
        post ("game" <//> var <//> "chat") $ \(gameid :: Int) -> do
            chat <- jsonBody' :: ApiAction Chat
            Spock.setStatus status200
 
        post ("game" <//> var <//> "move") $ \(gameid :: Int) -> do
            move <- jsonBody' :: ApiAction Move
            Spock.setStatus status200
        post ("game" <//> var <//> "resign") $ \(gameid :: Int) -> do
            Spock.setStatus status200
        post ("game" <//> var <//> "abort") $ \(gameid :: Int) -> do
            Spock.setStatus status200
        post ("game" <//> var <//> "draw" <//> var) $ \(gameid :: Int) (accept :: String) -> do
            Spock.setStatus status200

        post ("game" <//> "create") $ do
            Spock.setStatus status200