module Sound.Tidal.Target.Superdirt where

import Control.Concurrent.MVar

import Sound.Tidal.OSC.Target

-- superdirtTarget :: Target
-- superdirtTarget = Target {oName = "SuperDirt",
--                           oAddress = "127.0.0.1",
--                           oPort = 57120,
--                           oBusPort = Just 57110,
--                           oLatency = 0.2,
--                           oWindow = Nothing,
--                           oSchedule = Pre BundleStamp,
--                           oHandshake = True
--                          }

data SuperdirtConfig = SuperdirtConfig {

}

data SuperdirtTarget = SuperdirtTarget {
  sdTarget :: OSCTarget,
  sdBusTarget :: OSCTarget,
  sdBusses :: MVar [Int]
}

superdirt :: IO SuperdirtTarget
superdirt = do
              -- Start up OSC Targets
              target <- oscTarget "SuperDirt" "localhost" 57120
              busTarget <- oscTarget "SuperDirt Server" "localhost" 57110
              busses <- newMVar []
              -- Listen for handshake
              -- Send handshake to server
              send target "/dirt/handshake" []
              return SuperdirtTarget {
                sdTarget = target,
                sdBusTarget = busTarget,
                sdBusses = busses
                }

instance Target SuperdirtTarget where
  tick sd = tick (sdTarget sd) >> tick (sdBusTarget sd)