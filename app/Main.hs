{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Control.Concurrent
import Control.Exception
import qualified Data.ByteString as BS
import Control.Monad (forever, unless)

main :: IO ()
main = runServer Nothing "3000" talk
    where 
        talk conn = do
            msg <- recv conn 1024
            unless (BS.null msg) $ do
                sendAll conn msg
                talk conn

-- | Run a server on the given port and host. If host is Nothing, then the server will listen on all interfaces.
runServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runServer mhost port server = withSocketsDo $ do -- withSocketsDo is only needed for old versions of haskell on windows to initialize some library. Does nothing for other platforms.
    addr <- resolve
    bracket (open addr) close loop
    where   
        -- Resolve the address to bind to
        resolve :: IO AddrInfo
        resolve = do
            let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
            head <$> getAddrInfo (Just hints) mhost (Just port)
        -- Open a socket and initialize how many connections can be queued
        open :: AddrInfo -> IO Socket
        open addr = do
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            bind sock (addrAddress addr)
            listen sock 5
            return sock
        -- Start the loop of accepting connections, then create forks for each connection
        loop :: Socket -> IO a
        loop sock = forever $ do
            (conn, peer) <- accept sock
            putStrLn $ "Connected to " ++ show peer
            forkFinally (server conn) (\_ -> close conn)
