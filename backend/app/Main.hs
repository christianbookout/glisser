{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Glisser.Client.Client (runClient)
import Glisser.Server.Server (runServer) 
import System.Environment (getArgs)
import Glisser.WebServer.API (runWebServer)

main :: IO ()
main = do
    runWebServer
    -- args <- getArgs
    -- case args of
    --     [t, host, port] -> case t of 
    --         "c" -> runClient host port
    --         "s" -> runServer (Just host) port
    --         _ -> putStrLn "Invalid type. Use 'c' for client or 's' for server."
    --     _ -> putStrLn "Incorrect usage. Command is 'glisser <type> <host> <port>'"