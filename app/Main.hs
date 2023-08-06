{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Client (runClient)
import Server (runServer) 
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [t, host, port] -> case t of 
            "c" -> runClient host port
            "s" -> runServer (Just host) port
            _ -> putStrLn "Invalid type. Use 'c' for client or 's' for server."
        _ -> putStrLn "Incorrect usage. Command is 'glisser <type> <host> <port>'"