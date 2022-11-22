{-# LANGUAGE ExistentialQuantification #-}

module Sound.Tidal.OSC.Target
  ( Target,
    startTarget,
    tickTarget,
    endTarget,
    GenericTarget(..),
    Address(..),
    OSCShape,
    OSCTarget,
    oscTarget,
    OSCArg,
    required,
    optional,
    osc,
    send,
    sendPacket,
    contextShape,
  ) where

import qualified Data.Map.Strict as Map
import Data.Maybe
import Network.Socket hiding (socket)
import Network.Socket.ByteString hiding (send)

import Sound.Tidal.OSC.Core
import Sound.Tidal.Pattern
import Sound.Tidal.StreamTypes

class Target a where
  startTarget :: a -> IO ()
  startTarget _ = return ()

  tickTarget :: a -> Double -> ProcessedEvent -> IO ()

  endTarget :: a -> IO ()
  endTarget _ = return ()

-- | Wrapper type for a list of heterogeneous targets
data GenericTarget = forall a. Target a => GenericTarget a | EmptyTarget

type OSCShape = ProcessedEvent -> [Packet]

data OSCTarget = OSCTarget {
  oscName :: String,
  oscShape ::OSCShape,
  oscSocket :: Socket
}

instance Target OSCTarget where
  startTarget _ = return ()

  tickTarget t _ e
    = foldr ((>>) . (sendPacket t)) (return ()) $ oscShape t e
  
  endTarget _ = return ()

oscTarget :: String -> Address -> OSCShape -> IO OSCTarget
oscTarget name addr shape = OSCTarget name shape <$> socket
  where
    socket = do i <- resolve addr
                s <- openSocket i
                connect s (addrAddress i)
                return s

-- User-friendly shorthand for a single one-off message
send :: OSCTarget -> String -> [Value] -> IO ()
send target path args = sendPacket target $ Message path args

sendPacket :: OSCTarget -> Packet -> IO ()
sendPacket target packet = sendAll (oscSocket target) (encode packet)

contextShape :: String -> OSCShape
contextShape path pev = (map contextToMessage) . contextPosition . context $ ev
  where ev = peEvent pev
        contextToMessage :: ((Int,Int),(Int,Int)) -> Packet
        contextToMessage ((x, y), (x', y')) = Message path $ (VS ident):(VF delta):(VF cyc):(map VI [x,y,x',y'])
        ident = fromMaybe "unknown" $ Map.lookup "_id_" (value ev) >>= getS
        cyc = fromRational . start . wholeOrPart $ ev
        delta = error "Delta isn't currently implemented"

data Address = Address String Int | Port Int

type OSCArg = (String, (Maybe Value) -> Maybe Value)

required :: String -> OSCArg
required pName = (pName, id)

optional :: String -> Value -> OSCArg
optional pName val = (pName, Just . (maybe val id))

osc :: String -> [OSCArg] -> OSCShape
osc path args ev = maybeToList $ Message path <$> toData 
  where
    params = (value . peEvent) ev
    toData = sequence $ map (\(k, f) -> f $ Map.lookup k params) args

resolve :: Address -> IO AddrInfo
resolve (Port port) = resolve (Address "127.0.0.1" port)
resolve (Address address port)
  = head <$> getAddrInfo hints (Just address) (Just $ show port)
    where hints = Just defaultHints { addrSocketType = Datagram, addrFamily = AF_INET }