module Server (runServer) where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Control.Concurrent
import Control.Exception
import qualified Data.ByteString as BS
import Control.Monad (forever, unless)

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
        listen sock 5
        return sock
    loop :: Connections -> Socket -> IO ()
    loop conns sock = forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Connected to " ++ show peer
        modifyMVar_ conns (\connList -> return $ conn:connList)
        forkFinally (talk conns conn) (\_ -> closeConn conns conn)
    closeConn :: Connections -> Socket -> IO ()
    closeConn conns conn = do
        putStrLn "Closing connection" 
        modifyMVar_ conns $ \connList -> return (filter (/= conn) connList)
        close conn
    talk :: Connections -> Socket -> IO ()
    talk conns conn = forever $ do
        msg <- recv conn 1024
        unless (BS.null msg) $ do
            print msg
            connList <- readMVar conns
            mapM_ (`sendAll` msg) connList
