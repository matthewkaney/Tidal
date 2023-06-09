{-# LANGUAGE ExistentialQuantification #-}

{-
    Target.hs - Generic targets that can receive the output of Tidal patterns
    Copyright (C) 2020, Alex McLean and contributors

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

module Sound.Tidal.Target where

import Data.Map.Strict (Map)

import Sound.Tidal.ID
import Sound.Tidal.StreamTypes

-- |This typeclass describes a target, a destination for Tidal pattern
-- |events. Traditionally, this is an OSC server, but it can also include
-- |other types of actions.
class Target a where
  -- |An ID that identifies this target. Ids should ideally be descriptive
  -- |(e.g. "superdirt"), and must be unique for each target simultaneously
  -- |attached to a stream.
  targetID :: a -> ID

  -- |Dispatch an event. This takes the processed events from the current
  -- |tick of the stream and performs some action with them.
  targetTick :: a -> [ProcessedEvent] -> IO ()

  -- |Stop this target. This will be called when the target is removed from
  -- |a stream or the stream itself is stopped. This should undo any actions
  -- |taken in @targetStart@.
  targetStop :: a -> IO ()
  targetStop _ = return ()

-- |A stream can send to multiple different types of targets, so this
-- |wrapper abstracts away the specifics from the stream
data GenericTarget = forall a. Target a => GenericTarget a

-- |For convenience, @GenericTarget@ also behaves like a target
instance Target GenericTarget where
  targetID (GenericTarget a) = targetID a

  targetTick (GenericTarget a) = targetTick a

  targetStop (GenericTarget a) = targetStop a

-- |A collection of @GenericTarget@ instances
type TargetMap = Map ID GenericTarget