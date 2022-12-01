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
import qualified Data.Map.Strict as Map
import Data.Maybe
import Network.Socket hiding (socket)

import Sound.Tidal.OSC.Core
import Sound.Tidal.Pattern
import Sound.Tidal.Show ()

data OSCListener = OSCListener {
  oscAddress :: AddrInfo,
  oscSocket :: Socket,
  oscActionCount :: MVar Int,
  oscActions :: MVar ActionMap,
  oscDump :: MVar Bool,
  oscThread :: MVar (Maybe (Async ()))
}

type ActionMap = Map.Map String [(Int, OSCAction)]

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
                     thread <- async (socketReceive socket $ handleOSC l)
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
receive' l a f = reply' l a (\t vs -> f t vs >> return Nothing)

reply :: OSCListener -> String -> ([Value] -> IO (Maybe Packet)) -> IO (Int)
reply l a f = reply' l a (\_ -> f) -- TODO: Wait appropriate time

reply' :: OSCListener
       -> String
       -> (OSCTime -> [Value] -> IO (Maybe Packet))
       -> IO (Int)
reply' l a f = do i <- incrementMVar (oscActionCount l)
                  let f' = \t _ vs -> maybeToList <$> f t vs
                  modifyMVar_ (oscActions l)
                    (return . Map.insertWith (++) a [(i, f')])
                  return i

incrementMVar :: MVar Int -> IO Int
incrementMVar v = do val <- takeMVar v
                     putMVar v (val + 1)
                     return val

handleOSC :: OSCListener -> OSCAction
handleOSC l t a vs = fst <$> concurrently doActions dumpMessage
  where
    doActions :: IO [Packet]
    doActions = actions >>= (liftM concat) . (mapConcurrently doAction)
    actions :: IO [(Int, OSCAction)]
    actions = Map.findWithDefault [] a <$> readMVar (oscActions l)
    doAction :: (Int, OSCAction) -> IO [Packet]
    doAction (_, f) = f t a vs
    dumpMessage :: IO ()
    dumpMessage = readMVar (oscDump l) >>= flip when printMessage
    printMessage :: IO ()
    printMessage  = putStrLn ("OSC: " ++ a ++ " " ++ (show vs))