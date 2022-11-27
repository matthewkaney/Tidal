module Sound.Tidal.OSC.Listener (OSCListener, oscListener, receive) where

import Control.Concurrent.MVar
import qualified Data.Map.Strict as Map
import Network.Socket hiding (socket)

import Sound.Tidal.OSC.Core
import Sound.Tidal.Pattern

data OSCListener = OSCListener {
  oscSocket :: Socket,
  oscActions :: MVar ActionMap,
  oscDump :: MVar Bool
}

type OSCAction = OSCTime -> [Value] -> IO ()

type ActionMap = Map.Map String (Map.Map Int OSCAction)

oscListener :: Address -> IO (OSCListener)
oscListener _ = return OSCListener {}

receive :: OSCListener -> String -> ([Value] -> IO ()) -> IO (Int)
receive l a f = return 0