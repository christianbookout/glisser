{-# LANGUAGE OverloadedStrings #-}

module Client (runClient) where

import Network.Socket
import qualified Data.ByteString as BS
import Network.Socket.ByteString (recv, sendAll)
import Control.Exception
import Control.Monad
import Control.Concurrent (forkIO, threadDelay)

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
        -- Start a thread for sending messages
        _ <- forkIO $ forever $ do
            cnt <- BS.getLine
            sendAll conn cnt

        -- Start a thread for receiving messages
        _ <- forkIO $ forever $ do
            msg <- recv conn 1024
            unless (BS.null msg) $ putStrLn $ "[Message] " ++ show msg

        forever $ threadDelay maxBound