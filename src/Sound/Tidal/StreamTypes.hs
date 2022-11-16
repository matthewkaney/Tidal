module Sound.Tidal.StreamTypes where

import qualified Sound.OSC.FD as O
import qualified Sound.Tidal.Link as Link

import qualified Data.Map.Strict as Map
import Sound.Tidal.Pattern
import Sound.Tidal.Show ()

data PlayState = PlayState {pattern :: ControlPattern,
                            mute :: Bool,
                            solo :: Bool,
                            history :: [ControlPattern]
                           }
               deriving Show

type PatId = String
type PlayMap = Map.Map PatId PlayState

data TickState = TickState {
                    tickArc   :: Arc,
                    tickNudge :: Double
                   }
  deriving Show

data ProcessedEvent =
  ProcessedEvent {
    peHasOnset :: Bool,
    peEvent :: Event ValueMap,
    peCps :: Link.BPM,
    peDelta :: Link.Micros,
    peCycle :: Time,
    peOnWholeOrPart :: Link.Micros,
    peOnWholeOrPartOsc :: O.Time,
    peOnPart :: Link.Micros,
    peOnPartOsc :: O.Time
  }
