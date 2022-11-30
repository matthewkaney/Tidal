module Sound.Tidal.OSC.Listener
  ( OSCListener,
    oscListener,
    startListener,
    stopListener,
    dumpOSC,
    supportBroadcast,
    receive,
    receive',
    reply,
    reply' ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Network.Socket hiding (socket)
import Network.Socket.ByteString

import Sound.Tidal.OSC.Core
import Sound.Tidal.Pattern

data OSCListener = OSCListener {
  oscAddress :: AddrInfo,
  oscSocket :: Socket,
  oscActionCount :: MVar Int,
  oscActions :: MVar ActionMap,
  oscDump :: MVar Bool,
  oscThread :: MVar (Maybe (Async ()))
}

type OSCAction = OSCTime -> [Value] -> IO [Packet]

type ActionMap = Map.Map String (Map.Map Int OSCAction)

oscListener :: Address -> IO (OSCListener)
oscListener address
  = do info <- resolveUDP address
       socket <- openSocket info
       actionCount <- newMVar 0
       actions <- newMVar Map.empty
       dump <- newMVar False
       thread <- newMVar Nothing
       return OSCListener {
         oscAddress = info,
         oscSocket = socket,
         oscActionCount = actionCount,
         oscActions = actions,
         oscDump = dump,
         oscThread = thread
       }

startListener :: OSCListener -> IO ()
startListener l = do let socket = oscSocket l
                     bind socket (addrAddress $ oscAddress l)
                     thread <- async $ forever
                                 (recvFrom socket 8129 >>= dispatch l)
                     void $ swapMVar (oscThread l) (Just thread)

stopListener :: OSCListener -> IO ()
stopListener l = do swapMVar (oscThread l) Nothing >>= mapM_ cancel
                    close (oscSocket l)

dumpOSC :: OSCListener -> Bool -> IO ()
dumpOSC l v = void $ swapMVar (oscDump l) v

supportBroadcast :: OSCListener -> Bool -> IO ()
supportBroadcast l v = setSocketOption (oscSocket l) Broadcast (val v)
  where val False = 0
        val True = 1

receive :: OSCListener -> String -> ([Value] -> IO ()) -> IO (Int)
receive l a f = receive' l a (\_ -> f) -- TODO: Wait appropriate time

receive' :: OSCListener -> String -> (OSCTime -> [Value] -> IO ()) -> IO (Int)
receive' l a f = reply' l a (\t vs -> f t vs >> return [])

reply :: OSCListener -> String -> ([Value] -> IO [Packet]) -> IO (Int)
reply l a f = reply' l a (\_ -> f) -- TODO: Wait appropriate time

reply' :: OSCListener -> String -> OSCAction -> IO (Int)
reply' l a f = do i <- incrementMVar (oscActionCount l)
                  actions <- readMVar (oscActions l)
                  let newActions
                       = Map.insertWith Map.union a (Map.singleton i f) actions
                  putMVar (oscActions l) newActions
                  return i

incrementMVar :: MVar Int -> IO Int
incrementMVar v = do val <- takeMVar v
                     putMVar v (val + 1)
                     return val

dispatch :: OSCListener -> (ByteString, SockAddr) -> IO ()
dispatch l (rawData, sender) = (readMVar $ oscActions l)
                                 >>= handlePacket
                                 >>= sendReplies
  where
    handlePacket :: ActionMap -> IO [Packet]
    handlePacket aMap = handle aMap 0 (decode rawData)
    handle :: ActionMap -> OSCTime -> Packet -> IO [Packet]
    handle aMap _ (Bundle t ms) = liftM concat (mapM (handle aMap t) ms)
    handle aMap t (Message a vs) = dispatchMessage aMap a t vs
    sendReplies :: [Packet] -> IO ()
    sendReplies = mapM_ (\p -> sendTo (oscSocket l) (encode p) sender)

dispatchMessage :: ActionMap -> String -> OSCAction
dispatchMessage aMap addr t vs = maybe (return []) processActions actionList
  where
    actionList :: Maybe [(Int, OSCAction)]
    actionList = Map.toList <$> Map.lookup addr aMap
    processActions = (liftM concat) . (mapConcurrently callHandler)
    callHandler :: (Int, OSCAction) -> IO [Packet]
    callHandler (_, f) = f t vs