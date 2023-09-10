{-# LANGUAGE ScopedTypeVariables #-}
module Server (runServer) where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C8
import Control.Concurrent
import Control.Exception
import qualified Data.ByteString as BS
import Control.Monad (forever, unless)
import Glisser.Protocol (readCommand, Command (..))

type Connections = MVar [Socket]

-- | Run a server on the given port and host. If host is Nothing, then the server will listen on all interfaces.
runServer :: Maybe HostName -> ServiceName -> IO ()
runServer mhost port = withSocketsDo $ do -- withSocketsDo is only needed for old versions of haskell on windows to initialize some library. Does nothing for other platforms.
    addr <- resolve
    conns <- newMVar [] -- Create MVar holding a list of sockets
    bracket (open addr) close (loop conns)
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
        listen sock 4 -- 5 is the max, but we only listen for 4
        return sock
    loop :: Connections -> Socket -> IO ()
    loop conns sock = forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Connected to " ++ show peer
        modifyMVar_ conns (\connList -> return $ conn:connList)
        forkFinally (gameLoop conns conn) (\_ -> closeConn conns conn)
    closeConn :: Connections -> Socket -> IO ()
    closeConn conns conn = do
        putStrLn "Closing connection"
        modifyMVar_ conns $ \connList -> return (filter (/= conn) connList)
        close conn

-- | The game loop. This is where the server will receive messages from clients
-- and relay them to all other clients in the same game.
gameLoop :: Connections -> Socket -> IO ()
gameLoop conns conn = forever $ do
    msg <- C8.unpack <$> recv conn 1024
    case readCommand msg of
        Just (SetBoard board) -> return ()
        Just (MakeMove move) -> return ()
        Just TurnStart -> return ()
        Just (Error e) -> return ()
        Just (GameEnd winner) -> return ()
        Just Connect -> return ()
        Just Disconnect -> return ()
        Just JoinGame -> return ()
        Just LeaveGame -> return ()
        Just Forfeit -> return ()
        Nothing -> return ()
  where 
    sendToAll :: BS.ByteString -> IO ()
    sendToAll msg = do
        connList <- readMVar conns
        mapM_ (`sendAll` msg) connList
