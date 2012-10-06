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
       _ -> do putStrLn "Server usage: Ninjas server NUM_NINJAS [OPTIONS]"
               putStrLn "Client usage: Ninjas client [HOSTNAME [PORT]]"


launchServer :: [String] -> IO ()
launchServer args =
  case getOpt Permute opts args of
    (_ ,[] ,_)  -> usage
    (fs,[n],[]) -> serverMain (funs defaultServerEnv fs) (read n)
    (_ ,_  , errs) -> mapM_ putStrLn errs >> usage
  where
  usage = putStrLn (usageInfo "Ninjas server NUM_NINJAS [OPTIONS]" opts) >> exitFailure
  funs = foldl (\acc f -> f acc)
  opts = [Option [] ["port"]
                 (ReqArg (\n env -> env { serverPort = read n }) "NUM")
                 "Server port"
         ,Option [] ["npcs"]
                 (ReqArg (\n env -> env { npcCount   = read n }) "NUM")
                 "Number of NPCs"
         ,Option [] ["smokes"]
                 (ReqArg (\n env -> env { initialSmokebombs = read n }) "NUM")
                 "Number of initial smokebombs"
         ]

launchClient :: [String] -> IO ()
launchClient args = do
  user <- getUsername
  case getOpt Permute opts args of
    (fs, [h], [])  -> clientMain (funs defaultClientEnv{username=user
                                                       ,hostname=h
                                                       } fs)
    (fs, _, [])  -> clientMain (funs defaultClientEnv{username=user} fs)
    (_, _, errs) ->  mapM_ putStrLn errs >> usage
  where
  funs = foldl (\acc f -> f acc)
  usage = putStrLn (usageInfo "Usage: Ninjas client [HOSTNAME]" opts) >> exitFailure
  opts = [Option [] ["server"]
                 (ReqArg (\n env -> env { hostname = n }) "STRING")
                 "Server hostname"
         ,Option [] ["port"]
                 (ReqArg (\n env -> env { clientPort = read n }) "NUM")
                 "Server port"
         ,Option [] ["user"]
                 (ReqArg (\n env -> env { username = n }) "STRING")
                 "User Name"
         ]

getUsername :: IO String
getUsername =
  do env <- getEnvironment
     return $ fromMaybe (username defaultClientEnv)
            $ lookup "USER" env `mplus` lookup "USERNAME" env
