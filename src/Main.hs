{-# LANGUAGE RecordWildCards #-}

module Main where

import System.Environment

import Client (clientMain)
import Server (serverMain)

--------------------------------------------------------------------------------
npcCountDefault :: Int
npcCountDefault = 10

main :: IO ()
main =
  do args <- getArgs
     case args of
       ["server", playerCount]           -> serverMain (read playerCount) npcCountDefault
       ["server", playerCount, npcCount] -> serverMain (read playerCount) (read npcCount)
       ["client"]          -> clientMain "localhost"
       ["client",hostname] -> clientMain hostname
       _ -> do putStrLn "Server usage: Main server NUM_CLIENTS"
               putStrLn "Client usage: Main client [HOSTNAME]"
