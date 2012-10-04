module NetworkMessages where

import Control.Monad
import Data.Binary
import Data.Int
import Graphics.Gloss.Data.Picture
import Network (PortID(PortNumber))
import System.IO
import qualified Data.ByteString.Lazy as B

gamePort :: PortID
gamePort = PortNumber 16000

data Command
  = Move Point
  | Stop
  | Attack
  | Stun
  | Die
  deriving (Show, Read, Eq)
  
data ServerCommand
  = ServerCommand Int Command
  | SetWorld [(Point,Vector)]
  | ServerWaiting Int
  deriving (Show, Read)

hGetServerCommand :: Handle -> IO ServerCommand
hGetServerCommand = hGetCmd

hGetCommand :: Handle -> IO Command
hGetCommand = hGetCmd

hPutCommand :: Handle -> Command -> IO ()
hPutCommand = hPutCmd

hPutServerCommand :: Handle -> ServerCommand -> IO ()
hPutServerCommand = hPutCmd

hPutCmd :: Binary a => Handle -> a -> IO ()
hPutCmd h x = 
  do let bs = encode x
         n  = B.length bs
     B.hPutStr h (encode n)
     B.hPutStr h bs

hGetCmd :: Binary a => Handle -> IO a
hGetCmd h =
  do n <- decode `fmap` B.hGet h 8
     bs <- B.hGet h (fromIntegral (n :: Int64))
     return $ decode bs

instance Binary Command where
  put = putCommand
  get = getCommand

instance Binary ServerCommand where
  put = putServerCommand
  get = getServerCommand

putCommand :: Command -> Put
putCommand cmd =
  case cmd of
    Move pt -> putWord8 1 >> put pt
    Stop    -> putWord8 2
    Attack  -> putWord8 3
    Stun    -> putWord8 4
    Die     -> putWord8 5

getCommand :: Get Command
getCommand =
  do tag <- getWord8
     case tag of
       1 -> Move `fmap` get
       2 -> return Stop
       3 -> return Attack
       4 -> return Stun
       5 -> return Die
       _ -> error ("getCommand: bad tag " ++ show tag)

getServerCommand :: Get ServerCommand
getServerCommand =
  do tag <- getWord8
     case tag of
       1 -> return ServerCommand `ap` get `ap` get
       2 -> return SetWorld      `ap` get
       3 -> return ServerWaiting `ap` get
       _ -> error ("getServerCommand: bad tag " ++ show tag)

putServerCommand :: ServerCommand -> Put
putServerCommand cmd =
  case cmd of
    ServerCommand i c -> putWord8 1 >> put i >> put c
    SetWorld      xs  -> putWord8 2 >> put xs
    ServerWaiting i   -> putWord8 3 >> put i
