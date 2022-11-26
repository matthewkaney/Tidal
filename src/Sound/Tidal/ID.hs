{-# LANGUAGE FlexibleInstances #-}

module Sound.Tidal.ID (IDScope, getIDScope, ID, toID, intToID, valueToID) where

{-
    ID.hs - Polymorphic pattern identifiers
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

import Control.Concurrent.MVar

import Sound.Tidal.Pattern

newtype IDScope = IDScope { scopeNum :: Word } deriving (Eq, Ord)

defaultScope :: IDScope
defaultScope = IDScope 0

nextScope :: IO (MVar IDScope)
nextScope = newMVar (IDScope 1)

getIDScope :: IO (IDScope)
getIDScope = do scopeVar <- nextScope
                scope <- takeMVar scopeVar
                putMVar scopeVar (IDScope (scopeNum scope + 1))
                return scope

-- | Wrapper for literals that can be coerced to a string and used as an identifier.
-- | Similar to Show typeclass, but constrained to strings and integers and designed
-- | so that similar cases (such as 1 and "1") convert to the same value.
data ID = ID IDScope String deriving (Eq, Ord)

class IDLike a where
  toID :: a -> ID

instance IDLike ID where
  toID = id

instance IDLike String where
  toID = (ID $ IDScope 0)

instance IDLike Integer where
  toID = (ID $ IDScope 0) . show

intToID :: Integral a => a -> ID
intToID = toID . toInteger

valueToID :: Value -> Maybe ID
valueToID (VI a) = Just (intToID a)
valueToID (VS a) = Just (toID a)
valueToID _ = Nothing