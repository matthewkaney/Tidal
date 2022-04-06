module Sound.Tidal.OSC.Listener (createListener, setAction, listActions) where

import Data.Map.Strict (Map, empty, insert, keys)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, readMVar)
import Control.Monad (void)

import Sound.OSC.FD as O

type Action = String -> IO ()
type ActionMap = Map String Action

data OSCSocket = OSCSocket { actions :: MVar ActionMap }

createListener :: Integer -> IO (OSCSocket)
createListener port = do emptyActions <- newMVar empty
                         return $ OSCSocket emptyActions

setAction :: OSCSocket -> String -> Action -> IO ()
setAction l p a = void $ modifyMVar_ (actions l) (pure . (insert p a))

listActions :: OSCSocket -> IO [String]
listActions l = keys <$> (readMVar $ actions l)

--listenPort = 6011

--listen :: IO ()
--listen = do udp <- udpServer "127.0.0.1" listenPort
--            loop udp
--              where
--                loop udp = 
--                  do m <- recvMessage udp
--                     act udp m
--                     loop udp


--act :: UDP -> Maybe O.Message -> IO ()
--act _ _ = putStrLn "aha"
  
