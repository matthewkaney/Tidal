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
  sdAddress :: Address,
  sdBusAddress :: Address
}

data SuperdirtTarget = SuperdirtTarget {
  sdTarget :: OSCTarget,
  sdBusTarget :: OSCTarget
}

superdirt :: IO SuperdirtTarget
superdirt = do
              -- Start up OSC Targets
              busses <- newMVar []
              target <- oscTarget "SuperDirt" (Address "localhost" 57120) (sdShape busses)
              busTarget <- oscTarget "SuperDirt Server" (Address "localhost" 57110) (sdBusShape busses)
              -- Listen for handshake
              -- Send handshake to server
              send target "/dirt/handshake" []
              return SuperdirtTarget {
                sdTarget = target,
                sdBusTarget = busTarget
                }

instance Target SuperdirtTarget where
  tick sd = tick (sdTarget sd) >> tick (sdBusTarget sd)

mergeAddr :: Address -> Address -> Address
mergeAddr (Address a _) (Port p) = Address a p
mergeAddr _ addr = addr

sdShape :: MVar [Int] -> OSCShape
sdShape busses ev = []

sdBusShape :: MVar [Int] -> OSCShape
sdBusShape busses ev = []