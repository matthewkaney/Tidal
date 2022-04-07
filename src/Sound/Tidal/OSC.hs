{-
    OSC.hs - OSC interface for Tidal
    Copyright (C) 2022, Alex McLean and contributors

    This library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library.  If not, see <http://www.gnu.org/licenses/>.
-}

module Sound.Tidal.OSC (createListener, setAction, listActions) where

import Data.Map.Strict (Map, empty, insert, keys)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, readMVar)
import Control.Monad (void)

import Sound.Tidal.Pattern (Value)

import Sound.OSC.FD (udpServer, recvMessage, UDP)
import qualified Sound.OSC.FD as O (Message)

data Message = Message { path :: String, args :: [Value] }

type Action = Message -> IO (Maybe Message)
type ActionMap = Map String Action

data OSCSocket = OSCSocket { oActions :: MVar ActionMap, oUDP :: UDP }

createListener :: Int -> IO (OSCSocket)
createListener port = do udp <- udpServer "127.0.0.1" port
                         actions <- newMVar empty
                         let oscSocket = OSCSocket actions udp
                         listen oscSocket
                         return oscSocket

setAction :: OSCSocket -> String -> Action -> IO ()
setAction l p a = void $ modifyMVar_ (oActions l) (pure . (insert p a))

listActions :: OSCSocket -> IO [String]
listActions l = keys <$> (readMVar $ oActions l)

listen :: OSCSocket -> IO ()
listen sock = do message <- recvMessage $ oUDP sock
                 act message
                 listen sock


act :: Maybe O.Message -> IO ()
act _ = putStrLn "aha"
  
