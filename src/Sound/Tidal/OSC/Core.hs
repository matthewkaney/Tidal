module Sound.Tidal.OSC.Core
  ( Address(..),
    resolveUDP,
    OSCTime,
    Packet(..),
    encode,
    decode ) where

import Data.ByteString (ByteString)
import GHC.Float
import Network.Socket

import qualified Sound.OSC.Core as OSC

import Sound.Tidal.Pattern

data Address = Address String Int | Port Int

resolveUDP :: Address -> IO AddrInfo
resolveUDP (Port port) = resolveUDP (Address "127.0.0.1" port)
resolveUDP (Address address port)
  = head <$> getAddrInfo hints (Just address) (Just $ show port)
    where hints = Just defaultHints { addrSocketType = Datagram, addrFamily = AF_INET }

type OSCTime = OSC.Time

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

decode :: ByteString -> Packet
decode = fromPacket . OSC.decodePacket_strict

fromPacket :: OSC.Packet -> Packet
fromPacket (OSC.Packet_Bundle b) = fromBundle b
fromPacket (OSC.Packet_Message m) = fromMessage m

fromBundle :: OSC.Bundle -> Packet
fromBundle (OSC.Bundle t ms) = Bundle t (map fromMessage ms)

fromMessage :: OSC.Message -> Packet
fromMessage (OSC.Message a vs) = Message a (map fromDatum vs)

fromDatum :: OSC.Datum -> Value
fromDatum (OSC.Int32 x) = VI $ fromIntegral x
fromDatum (OSC.Int64 x) = VI $ fromIntegral x
fromDatum (OSC.Float x) = VF $ float2Double x
fromDatum (OSC.Double x) = VF x
fromDatum (OSC.ASCII_String x) = VS $ OSC.ascii_to_string x
fromDatum (OSC.Blob x) = VX $ OSC.blob_unpack x
fromDatum (OSC.TimeStamp x) = VF x -- TODO: Convert this to local Tidal time?
fromDatum (OSC.Midi (OSC.MIDI x0 x1 x2 x3)) = VList $ map (VI . fromIntegral) [x0, x1, x2, x3]