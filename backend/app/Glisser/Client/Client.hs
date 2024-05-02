{-# LANGUAGE OverloadedStrings #-}

module Glisser.Client.Client (runClient) where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Control.Exception
import Control.Monad
import Control.Concurrent (forkIO, threadDelay)
import Glisser.Client.Protocol (Command (..))
import Text.Read (readMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Control.Concurrent.STM (newTVarIO)
import Control.Monad.STM (atomically)

runClient :: HostName -> ServiceName -> IO ()
runClient host port = withSocketsDo $ do
    addr <- resolve
    bracket (open addr) close talk
  where
    -- Resolve the server address
    resolve :: IO AddrInfo
    resolve = do
        let hints = defaultHints { addrFlags = [], addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    -- Open a socket
    open :: AddrInfo -> IO Socket
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock (addrAddress addr)
        return sock
    -- Send and receive data
    talk :: Socket -> IO ()
    talk conn = do
        -- gameState <- newTVarIO defaultGameState

        -- Start a thread for sending messages
        _ <- forkIO $ forever $ do
            putStrLn "Enter a command: "
            msg <- getLine
            sendAll conn (E.encodeUtf8 $ T.pack msg)
            -- case readMaybe msg :: Maybe Command of
            --     Just cmd -> do 
            --         putStrLn "sending command"
            --         sendAll conn (E.encodeUtf8 $ T.pack $ show cmd)
            --         putStrLn "finished"
            --     Nothing -> putStrLn $ "Invalid command. Please retry: " ++ show msg

        -- Start a thread for receiving messages
        _ <- forkIO $ forever $ do
            msg <- recv conn 1024
            case readMaybe (T.unpack $ E.decodeUtf8 msg) :: Maybe Command of
                Just cmd -> print cmd
                Nothing  -> putStrLn $ "Received invalid command from server: " ++ show msg

        forever $ threadDelay maxBound