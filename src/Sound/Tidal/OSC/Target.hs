module Sound.Tidal.OSC.Target
  ( Target, tick, OSCTarget, oscTarget, send, sendPacket ) where

import Network.Socket hiding (socket)
import Network.Socket.ByteString hiding (send)

import Sound.Tidal.OSC.Core
import Sound.Tidal.Pattern

class Target a where
  tick :: a -> IO ()

type OSCShape = Event ValueMap -> [Packet]

data OSCTarget = OSCTarget {
  oscName :: String,
  oscSocket :: Socket
}

instance Target OSCTarget where
  tick t = return ()

oscTarget :: String -> String -> Int -> IO OSCTarget
oscTarget name addr port = OSCTarget name <$> socket
  where
    hints = Just defaultHints { addrSocketType = Datagram, addrFamily = AF_INET }
    socket = do i:_ <- getAddrInfo hints (Just addr) (Just $ show port)
                s <- openSocket i
                connect s (addrAddress i)
                return s

-- User-friendly shorthand for a single one-off message
send :: OSCTarget -> String -> [Value] -> IO ()
send target path args = sendPacket target $ Message path args

sendPacket :: OSCTarget -> Packet -> IO ()
sendPacket target packet = sendAll (oscSocket target) (encode packet)

-- TODO: This doesn't fully implement existing context messages, because
-- the pattern id, context, and event aren't available from here...
contextTarget :: String -> OSCShape
contextTarget path = (map contextToMessage) . contextPosition . context
  where contextToMessage :: ((Int,Int),(Int,Int)) -> Packet
        contextToMessage ((x, y), (x', y'))
            = Message path (map VI [x,y,x',y'])
-- toOSC _ pe (OSCContext oscpath)
--   = map cToM $ contextPosition $ context $ peEvent pe
--   where cToM :: ((Int,Int),(Int,Int)) -> (Double, Bool, O.Message)
--         cToM ((x, y), (x',y')) = (ts,
--                                   False, -- bus message ?
--                                   O.Message oscpath $ (O.string ident):(O.float (peDelta pe)):(O.float cyc):(map O.int32 [x,y,x',y'])
--                                  )