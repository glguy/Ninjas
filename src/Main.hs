{-# LANGUAGE RecordWildCards #-}

module Main where

import System.Environment

import Client (clientMain)
import Server (serverMain)

--------------------------------------------------------------------------------

main :: IO ()
main =
  do args <- getArgs
     case args of
       ["server",n]        -> serverMain (read n)
       ["client"]          -> clientMain "localhost"
       ["client",hostname] -> clientMain hostname
       _ -> do putStrLn "Server usage: Main server NUM_CLIENTS"
               putStrLn "Client usage: Main client [HOSTNAME]"
