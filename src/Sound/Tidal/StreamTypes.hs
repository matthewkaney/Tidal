module Sound.Tidal.StreamTypes where

import qualified Data.Map.Strict as Map

import qualified Sound.Osc.Fd as O
import qualified Sound.Tidal.Link as Link

import Sound.Tidal.Types
import Sound.Tidal.Show ()

data PlayState = PlayState {pattern :: ControlSignal,
                            mute :: Bool,
                            solo :: Bool,
                            history :: [ControlSignal]
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
    peHasOnset         :: Bool,
    peEvent            :: Event ValueMap,
    peCps              :: Link.BPM,
    peDelta            :: Link.Micros,
    peCycle            :: Time,
    peOnWholeOrPart    :: Link.Micros,
    peOnWholeOrPartOsc :: O.Time,
    peOnPart           :: Link.Micros,
    peOnPartOsc        :: O.Time
  }