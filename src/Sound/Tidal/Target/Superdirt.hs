module Sound.Tidal.Target.Superdirt where

import qualified Data.Map.Strict as Map
import Control.Concurrent.MVar

import Sound.Tidal.OSC.Core
import Sound.Tidal.OSC.Target
import Sound.Tidal.Pattern
import Sound.Tidal.StreamTypes

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
  startTarget sd = startTarget (sdTarget sd) >> startTarget (sdBusTarget sd)
  tickTarget sd = tickTarget (sdTarget sd) >> tickTarget (sdBusTarget sd)
  endTarget sd = endTarget (sdTarget sd) >> endTarget (sdBusTarget sd)

mergeAddr :: Address -> Address -> Address
mergeAddr (Address a _) (Port p) = Address a p
mergeAddr _ addr = addr

sdShape :: MVar [Int] -> OSCShape
sdShape busses ev = []
  -- where
  --   params = (value . peEvent) ev
  --   (playmap, busmap) = Map.partitionWithKey (\k _ -> null k || head k /= '^') params
  --   playmap' = Map.union (Map.mapKeys tail $ Map.map (\(VI i) -> VS ('c':(show $ toBus i))) busmap) playmap

sdBusShape :: MVar [Int] -> OSCShape
sdBusShape busses ev = (toMessage . toArgPairs) params
  where
    params = (value . peEvent) ev
    toMessage [] = []
    toMessage ps = [Message "/c_set" ps]
    toArgPairs = Map.foldrWithKey appendArgPair []
    appendArgPair ('^':k) b@(VI _) ps
      = maybe [] ((b:) . singleton) (Map.lookup k params) ++ ps
    appendArgPair _ _ ps = ps

singleton :: a -> [a]
singleton a = a:[]