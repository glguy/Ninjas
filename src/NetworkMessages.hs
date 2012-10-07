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
  = Move Point Point
  | Stop 
  | Attack
  | Stun
  | Die
  deriving (Show, Read, Eq)

data ClientCommand
  = ClientCommand Command
  | ClientSmoke
  | ClientJoin String
  | NewGame
  deriving (Show, Read, Eq)
  
data ServerCommand
  = ServerCommand Int Command
  | SetWorld [(Point,Vector)]
  | ServerWaiting Int
  | ServerMessage String
  | ServerSmoke Point
  | ServerDing
  | ServerReady
  deriving (Show, Read)

hGetClientCommand :: Handle -> IO ClientCommand
hGetClientCommand = hGetCmd

hPutClientCommand :: Handle -> ClientCommand -> IO ()
hPutClientCommand h x =
  do let bs = encode x
         n  = encode (B.length bs)
     B.hPutStr h n
     B.hPutStr h bs

hGetServerCommand :: Handle -> IO ServerCommand
hGetServerCommand = hGetCmd

newtype ServerPacket = ServerPacket B.ByteString

hPutServerPacket :: Handle -> ServerPacket -> IO ()
hPutServerPacket h (ServerPacket bs) = B.hPutStr h bs

mkServerPacket :: ServerCommand -> ServerPacket
mkServerPacket x = ServerPacket (B.append n bs)
  where
  bs = encode x
  n  = encode (B.length bs)

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

instance Binary ClientCommand where
  put = putClientCommand
  get = getClientCommand

putCommand :: Command -> Put
putCommand cmd =
  case cmd of
    Move pt1 pt2 -> putWord8 1 >> put pt1 >> put pt2
    Stop    -> putWord8 2
    Attack  -> putWord8 3
    Stun    -> putWord8 4
    Die     -> putWord8 5

getCommand :: Get Command
getCommand =
  do tag <- getWord8
     case tag of
       1 -> Move `fmap` get `ap` get
       2 -> return Stop
       3 -> return Attack
       4 -> return Stun
       5 -> return Die
       _ -> error ("getCommand: bad tag " ++ show tag)

putClientCommand :: ClientCommand -> Put
putClientCommand cmd =
  case cmd of
    ClientCommand c -> putWord8 1 >> put c
    ClientJoin name -> putWord8 2 >> put name
    ClientSmoke     -> putWord8 3
    NewGame         -> putWord8 4

getClientCommand :: Get ClientCommand
getClientCommand =
  do tag <- getWord8
     case tag of
       1 -> return ClientCommand `ap` get
       2 -> return ClientJoin    `ap` get
       3 -> return ClientSmoke
       4 -> return NewGame
       _ -> error ("getClientCommand: bad tag " ++ show tag)

getServerCommand :: Get ServerCommand
getServerCommand =
  do tag <- getWord8
     case tag of
       1 -> return ServerCommand `ap` get `ap` get
       2 -> return SetWorld      `ap` get
       3 -> return ServerWaiting `ap` get
       4 -> return ServerMessage `ap` get
       5 -> return ServerDing
       6 -> return ServerSmoke   `ap` get
       7 -> return ServerReady
       _ -> error ("getServerCommand: bad tag " ++ show tag)

putServerCommand :: ServerCommand -> Put
putServerCommand cmd =
  case cmd of
    ServerCommand i c -> putWord8 1 >> put i >> put c
    SetWorld      xs  -> putWord8 2 >> put xs
    ServerWaiting i   -> putWord8 3 >> put i
    ServerMessage txt -> putWord8 4 >> put txt
    ServerDing        -> putWord8 5
    ServerSmoke pt    -> putWord8 6 >> put pt
    ServerReady       -> putWord8 7
