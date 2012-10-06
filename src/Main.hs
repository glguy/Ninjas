{-# LANGUAGE RecordWildCards #-}

module Main where

import System.Environment
import System.Exit
import System.Console.GetOpt

import Client (clientMain)
import Server (ServerEnv(..), defaultEnv, serverMain)

--------------------------------------------------------------------------------

main :: IO ()
main =
  do args <- getArgs
     case args of
       "server" : args' -> launchServer args'
       ["client"]          -> clientMain "localhost"
       ["client",hostname] -> clientMain hostname
       _ -> do putStrLn "Server usage: Main server NUM_NINJAS [OPTIONS]"
               putStrLn "Client usage: Main client [HOSTNAME]"


launchServer :: [String] -> IO ()
launchServer args =
  case getOpt Permute opts args of
    (_ ,[] ,_)  -> usage
    (fs,[n],[]) -> serverMain (funs defaultEnv fs) (read n)
    (_ ,_  , errs) -> mapM_ putStrLn errs >> usage
  where
  usage = putStrLn (usageInfo "Ninjas server NUM_NINJAS [OPTIONS]" opts) >> exitFailure
  funs = foldl (\acc f -> f acc)
  opts = [Option [] ["port"]
                 (ReqArg (\n env -> env { serverPort = read n }) "NUM")
                 "Server listening port"
         ,Option [] ["npcs"]
                 (ReqArg (\n env -> env { npcCount   = read n }) "NUM")
                 "Number of NPCs"
         ,Option [] ["smokes"]
                 (ReqArg (\n env -> env { initialSmokebombs = read n }) "NUM")
                 "Number of initial smokebombs"
         ]
