{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (mplus)
import Data.Maybe (fromMaybe)
import System.Environment
import System.Exit
import System.Console.GetOpt

import Client (ClientEnv(..), defaultClientEnv, clientMain)
import Server (ServerEnv(..), defaultServerEnv, serverMain)

--------------------------------------------------------------------------------

main :: IO ()
main =
  do args <- getArgs
     case args of
       "server" : args' -> launchServer args'
       "client" : args' -> launchClient args'
       _                -> usage

usage :: IO a
usage =
  do putStrLn "Usage:"
     putStr $ usageInfo "Ninjas server [FLAGS] NUM_NINJAS"        serverOpts
     putStr $ usageInfo "Ninjas client [FLAGS] [HOSTNAME [PORT]]" clientOpts
     exitFailure

launchServer :: [String] -> IO ()
launchServer args =
  case getOpt Permute serverOpts args of
    (fs, [n], []) -> serverMain (funs defaultServerEnv fs) (read n)
    (_ , _  , es) -> mapM_ putStrLn es >> usage
  where
  funs = foldl (\acc f -> f acc)

serverOpts :: [OptDescr (ServerEnv -> ServerEnv)]
serverOpts =
  [ Option [] ["port"]
           (ReqArg (\n env -> env { serverPort = read n }) "NUM")
           "Server port"
  , Option [] ["npcs"]
           (ReqArg (\n env -> env { npcCount   = read n }) "NUM")
           "Number of NPCs"
  , Option [] ["smokes"]
           (ReqArg (\n env -> env { initialSmokebombs = read n }) "NUM")
           "Number of initial smokebombs"
  ]

launchClient :: [String] -> IO ()
launchClient args =
  do user <- getUsername
     case getOpt Permute clientOpts args of
       (fs, [h], []) -> clientMain (funs defaultClientEnv { username=user
                                                           , hostname=h
                                                           } fs)
       (fs, _  , []) -> clientMain (funs defaultClientEnv{username=user} fs)
       (_ , _  , es) -> mapM_ putStrLn es >> usage
  where
  funs = foldl (\acc f -> f acc)

clientOpts :: [OptDescr (ClientEnv -> ClientEnv)]
clientOpts =
  [ Option [] ["server"]
           (ReqArg (\n env -> env { hostname = n }) "STRING")
           "Server hostname"
  , Option [] ["port"]
           (ReqArg (\n env -> env { clientPort = read n }) "NUM")
           "Server port"
  , Option [] ["user"]
           (ReqArg (\n env -> env { username = n }) "STRING")
           "User Name"
  ]

getUsername :: IO String
getUsername =
  do env <- getEnvironment
     return $ fromMaybe (username defaultClientEnv)
            $ lookup "USER" env `mplus` lookup "USERNAME" env
