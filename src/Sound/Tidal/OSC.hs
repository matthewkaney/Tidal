module Sound.Tidal.OSC where

{-
    OSC.hs - Wrapper around HOSC for easier interfacing with Tidal values
    Copyright (C) 2021, Alex McLean and contributors

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

import GHC.Float (float2Double)
import Sound.OSC.FD

import Sound.Tidal.Pattern

toDatum :: Value -> Datum
toDatum (VF x) = float x
toDatum (VN x) = float x
toDatum (VI x) = int32 x
toDatum (VS x) = string x
toDatum (VR x) = float $ ((fromRational x) :: Double)
toDatum (VB True) = int32 (1 :: Int)
toDatum (VB False) = int32 (0 :: Int)
toDatum (VX xs) = Blob $ blob_pack xs
toDatum _ = error "toDatum: unhandled value"

fromDatum :: Datum -> Value
fromDatum (Float x) = VF $ float2Double x
fromDatum (Double x) = VF x
fromDatum (Int32 x) = VI $ fromIntegral x
fromDatum (ASCII_String x) = VS $ ascii_to_string x
fromDatum _ = error "fromDatum: unhandled value"