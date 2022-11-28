module Sound.Tidal.OSC.Listener
  ( OSCListener,
    oscListener,
    startListener,
    stopListener,
    dumpOSC,
    supportBroadcast,
    receive ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad
import Data.ByteString
import qualified Data.Map.Strict as Map
import Network.Socket hiding (socket)
import Network.Socket.ByteString

import Sound.Tidal.OSC.Core
import Sound.Tidal.Pattern

data OSCListener = OSCListener {
  oscAddress :: AddrInfo,
  oscSocket :: Socket,
  oscActions :: MVar ActionMap,
  oscDump :: MVar Bool,
  oscThread :: MVar (Maybe (Async ()))
}

type OSCAction = OSCTime -> [Value] -> IO ()

type ActionMap = Map.Map String (Map.Map Int OSCAction)

oscListener :: Address -> IO (OSCListener)
oscListener address
  = do info <- resolveUDP address
       socket <- openSocket info
       actions <- newMVar Map.empty
       dump <- newMVar False
       thread <- newMVar Nothing
       return OSCListener {
         oscAddress = info,
         oscSocket = socket,
         oscActions = actions,
         oscDump = dump,
         oscThread = thread
       }

startListener :: OSCListener -> IO ()
startListener l = do let socket = oscSocket l
                     bind socket (addrAddress $ oscAddress l)
                     thread <- async $ forever (recvFrom socket 8129 >>= dispatch)
                     void $ swapMVar (oscThread l) (Just thread)

stopListener :: OSCListener -> IO ()
stopListener l = return ()

dumpOSC :: OSCListener -> Bool -> IO ()
dumpOSC l v = void $ swapMVar (oscDump l) v

supportBroadcast :: OSCListener -> Bool -> IO ()
supportBroadcast l v = setSocketOption (oscSocket l) Broadcast (val v)
  where val False = 0
        val True = 1

receive :: OSCListener -> String -> ([Value] -> IO ()) -> IO (Int)
receive l a f = return 0

dispatch :: (ByteString, SockAddr) -> IO ()
dispatch _ = return ()