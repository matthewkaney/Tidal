module Sound.Tidal.OSC.Core ( Packet(..), encode) where

import Data.ByteString (ByteString)

import qualified Sound.OSC.Core as OSC

import Sound.Tidal.Pattern

data Packet = Message String [Value] | Bundle OSC.Time [Packet]

encode :: Packet -> ByteString
encode = OSC.encodePacket_strict . toPacket

toPacket :: Packet -> OSC.Packet
toPacket (Message addr args) = OSC.p_message addr (toData args)
toPacket (Bundle time packets) = OSC.p_bundle time (map toMessage packets)
  where toMessage :: Packet -> OSC.Message
        toMessage (Message addr args) = OSC.Message addr (toData args)
        toMessage (Bundle _ _) = error "Nested bundles aren't currently supported"

toData :: [Value] -> [OSC.Datum]
toData = map toDatum

toDatum :: Value -> OSC.Datum
toDatum (VF x) = OSC.float x
toDatum (VN x) = OSC.float x
toDatum (VI x) = OSC.int32 x
toDatum (VS x) = OSC.string x
toDatum (VR x) = OSC.float $ ((fromRational x) :: Double)
toDatum (VB True) = OSC.int32 (1 :: Int)
toDatum (VB False) = OSC.int32 (0 :: Int)
toDatum (VX xs) = OSC.Blob $ OSC.blob_pack xs
toDatum _ = error "toDatum: unhandled value"