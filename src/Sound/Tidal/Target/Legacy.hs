{-# LANGUAGE ScopedTypeVariables #-}

module Sound.Tidal.Target.Legacy where

import           Control.Applicative ((<|>))
import           Control.Concurrent
import qualified Control.Exception as E
import           Control.Monad (forM_, when)
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Word
import           Foreign.C.Types
import           System.IO (hPutStrLn, stderr)

import qualified Sound.OSC.FD as O
import qualified Network.Socket as N

import           Sound.Tidal.Config
import qualified Sound.Tidal.OSC.Target as New
import           Sound.Tidal.Pattern
import           Sound.Tidal.Show ()
import           Sound.Tidal.StreamTypes
import qualified Sound.Tidal.Tempo as T
import           Sound.Tidal.Utils ((!!!))

data Cx = Cx {cxTarget :: Target,
              cxUDP :: O.UDP,
              cxOSCs :: [OSC],
              cxAddr :: N.AddrInfo,
              cxBusAddr :: Maybe N.AddrInfo,
              cxBusses :: MVar [Int],
              cxVerbose :: Bool
             }
  -- deriving (Show)

data StampStyle = BundleStamp
                | MessageStamp
  deriving (Eq, Show)

data Schedule = Pre StampStyle
              | Live
  deriving (Eq, Show)

data Target = Target {oName :: String,
                      oAddress :: String,
                      oPort :: Int,
                      oBusPort :: Maybe Int,
                      oLatency :: Double,
                      oWindow :: Maybe Arc,
                      oSchedule :: Schedule,
                      oHandshake :: Bool
                     }
                 deriving Show

data Args = Named {requiredArgs :: [String]}
          | ArgList [(String, Maybe Value)]
         deriving Show

data OSC = OSC {path :: String,
                args :: Args
               }
         | OSCContext {path :: String}
         deriving Show

sDefault :: String -> Maybe Value
sDefault x = Just $ VS x
fDefault :: Double -> Maybe Value
fDefault x = Just $ VF x
rDefault :: Rational -> Maybe Value
rDefault x = Just $ VR x
iDefault :: Int -> Maybe Value
iDefault x = Just $ VI x
bDefault :: Bool -> Maybe Value
bDefault x = Just $ VB x
xDefault :: [Word8] -> Maybe Value
xDefault x = Just $ VX x

required :: Maybe Value
required = Nothing

superdirtTarget :: Target
superdirtTarget = Target {oName = "SuperDirt",
                          oAddress = "127.0.0.1",
                          oPort = 57120,
                          oBusPort = Just 57110,
                          oLatency = 0.2,
                          oWindow = Nothing,
                          oSchedule = Pre BundleStamp,
                          oHandshake = True
                         }

superdirtShape :: OSC
superdirtShape = OSC "/dirt/play" $ Named {requiredArgs = ["s"]}

dirtTarget :: Target
dirtTarget = Target {oName = "Dirt",
                     oAddress = "127.0.0.1",
                     oPort = 7771,
                     oBusPort = Nothing,
                     oLatency = 0.02,
                     oWindow = Nothing,
                     oSchedule = Pre MessageStamp,
                     oHandshake = False
                    }

dirtShape :: OSC
dirtShape = OSC "/play" $ ArgList [("cps", fDefault 0),
                                   ("s", required),
                                   ("offset", fDefault 0),
                                   ("begin", fDefault 0),
                                   ("end", fDefault 1),
                                   ("speed", fDefault 1),
                                   ("pan", fDefault 0.5),
                                   ("velocity", fDefault 0.5),
                                   ("vowel", sDefault ""),
                                   ("cutoff", fDefault 0),
                                   ("resonance", fDefault 0),
                                   ("accelerate", fDefault 0),
                                   ("shape", fDefault 0),
                                   ("kriole", iDefault 0),
                                   ("gain", fDefault 1),
                                   ("cut", iDefault 0),
                                   ("delay", fDefault 0),
                                   ("delaytime", fDefault (-1)),
                                   ("delayfeedback", fDefault (-1)),
                                   ("crush", fDefault 0),
                                   ("coarse", iDefault 0),
                                   ("hcutoff", fDefault 0),
                                   ("hresonance", fDefault 0),
                                   ("bandf", fDefault 0),
                                   ("bandq", fDefault 0),
                                   ("unit", sDefault "rate"),
                                   ("loop", fDefault 0),
                                   ("n", fDefault 0),
                                   ("attack", fDefault (-1)),
                                   ("hold", fDefault 0),
                                   ("release", fDefault (-1)),
                                   ("orbit", iDefault 0) -- ,
                                   -- ("id", iDefault 0)
                                  ]

legacyCx :: Config -> (Target, [OSC]) -> IO Cx
legacyCx config (target, os)
  = do busses <- newMVar []
       remote_addr <- resolve (oAddress target) (show $ oPort target)
       remote_bus_addr <- if isJust $ oBusPort target
                            then Just <$> resolve (oAddress target) (show $ fromJust $ oBusPort target)
                            else return Nothing
       let broadcast = if (cCtrlBroadcast config) then 1 else 0
           verbose = cVerbose config
       u <- O.udp_socket (\sock sockaddr -> do N.setSocketOption sock N.Broadcast broadcast
                                               N.connect sock sockaddr
                         ) (oAddress target) (oPort target)
       return $ Cx {cxUDP = u, cxAddr = remote_addr, cxBusAddr = remote_bus_addr, cxTarget = target, cxOSCs = os, cxBusses = busses, cxVerbose = verbose}

instance New.Target Cx where
  startTarget _ = return ()
  
  nudgeTarget _ _ = return ()

  tickTarget cx nudge ev
    = do busses <- readMVar (cxBusses cx)
         let target = cxTarget cx
             oscs = cxOSCs cx
             -- Latency is configurable per target.
             latency = oLatency target
             ms = concatMap (toOSC busses ev) oscs
         -- send the events to the OSC target
         forM_ ms $ \ m -> (do
           send (Just $ cxUDP cx) cx latency nudge m) `E.catch` \ (e :: E.SomeException) -> do
           hPutStrLn stderr $ "Failed to send. Is the '" ++ oName target ++ "' target running? " ++ show e

  endTarget _ = return ()

resolve :: String -> String -> IO N.AddrInfo
resolve host port = do let hints = N.defaultHints { N.addrSocketType = N.Stream }
                       addr:_ <- N.getAddrInfo (Just hints) (Just host) (Just port)
                       return addr

sendHandshake :: Cx -> IO ()
sendHandshake cx = if (oHandshake $ cxTarget cx)
                      then sendO False (Just $ cxUDP cx) cx $ O.Message "/dirt/handshake" []
                      else return ()

sendO :: Bool -> (Maybe O.UDP) -> Cx -> O.Message -> IO ()
sendO isBusMsg (Just listen) cx msg = O.sendTo listen (O.Packet_Message msg) (N.addrAddress addr)
  where addr | isBusMsg && isJust (cxBusAddr cx) = fromJust $ cxBusAddr cx
             | otherwise = cxAddr cx
sendO _ Nothing cx msg = O.sendMessage (cxUDP cx) msg

sendBndl :: Bool -> (Maybe O.UDP) -> Cx -> O.Bundle -> IO ()
sendBndl isBusMsg (Just listen) cx bndl = O.sendTo listen (O.Packet_Bundle bndl) (N.addrAddress addr)
  where addr | isBusMsg && isJust (cxBusAddr cx) = fromJust $ cxBusAddr cx
             | otherwise = cxAddr cx
sendBndl _ Nothing cx bndl = O.sendBundle (cxUDP cx) bndl

toDatum :: Value -> O.Datum
toDatum (VF x) = O.float x
toDatum (VN x) = O.float x
toDatum (VI x) = O.int32 x
toDatum (VS x) = O.string x
toDatum (VR x) = O.float $ ((fromRational x) :: Double)
toDatum (VB True) = O.int32 (1 :: Int)
toDatum (VB False) = O.int32 (0 :: Int)
toDatum (VX xs) = O.Blob $ O.blob_pack xs
toDatum _ = error "toDatum: unhandled value"
  
toData :: OSC -> Event ValueMap -> Maybe [O.Datum]
toData (OSC {args = ArgList as}) e = fmap (fmap (toDatum)) $ sequence $ map (\(n,v) -> Map.lookup n (value e) <|> v) as
toData (OSC {args = Named rqrd}) e
  | hasRequired rqrd = Just $ concatMap (\(n,v) -> [O.string n, toDatum v]) $ Map.toList $ value e
  | otherwise = Nothing
  where hasRequired [] = True
        hasRequired xs = null $ filter (not . (`elem` ks)) xs
        ks = Map.keys (value e)
toData _ _ = Nothing

substitutePath :: String -> ValueMap -> Maybe String
substitutePath str cm = parse str
  where parse [] = Just []
        parse ('{':xs) = parseWord xs
        parse (x:xs) = do xs' <- parse xs
                          return (x:xs')
        parseWord xs | b == [] = getString cm a
                     | otherwise = do v <- getString cm a
                                      xs' <- parse (tail b)
                                      return $ v ++ xs'
          where (a,b) = break (== '}') xs

getString :: ValueMap -> String -> Maybe String
getString cm s = (simpleShow <$> Map.lookup param cm) <|> defaultValue dflt
                      where (param, dflt) = break (== '=') s
                            simpleShow :: Value -> String
                            simpleShow (VS str) = str
                            simpleShow (VI i) = show i
                            simpleShow (VF f) = show f
                            simpleShow (VN n) = show n
                            simpleShow (VR r) = show r
                            simpleShow (VB b) = show b
                            simpleShow (VX xs) = show xs
                            simpleShow (VState _) = show "<stateful>"
                            simpleShow (VPattern _) = show "<pattern>"
                            simpleShow (VList _) = show "<list>"
                            defaultValue :: String -> Maybe String
                            defaultValue ('=':dfltVal) = Just dfltVal
                            defaultValue _ = Nothing

toOSC :: [Int] -> ProcessedEvent -> OSC -> [(Double, Bool, O.Message)]
toOSC busses pe osc@(OSC _ _)
  = catMaybes (playmsg:busmsgs)
      -- playmap is a ValueMap where the keys don't start with ^ and are not ""
      -- busmap is a ValueMap containing the rest of the keys from the event value
      -- The partition is performed in order to have special handling of bus ids.
      where
        (playmap, busmap) = Map.partitionWithKey (\k _ -> null k || head k /= '^') $ val pe
        -- Map in bus ids where needed.
        --
        -- Bus ids are integers
        -- If busses is empty, the ids to send are directly contained in the the values of the busmap.
        -- Otherwise, the ids to send are contained in busses at the indices of the values of the busmap.
        -- Both cases require that the values of the busmap are only ever integers,
        -- that is, they are Values with constructor VI
        -- (but perhaps we should explicitly crash with an error message if it contains something else?).
        -- Map.mapKeys tail is used to remove ^ from the keys.
        -- In case (value e) has the key "", we will get a crash here.
        playmap' = Map.union (Map.mapKeys tail $ Map.map (\(VI i) -> VS ('c':(show $ toBus i))) busmap) playmap
        val = value . peEvent
        -- Only events that start within the current nowArc are included
        playmsg | peHasOnset pe = do
                  -- If there is already cps in the event, the union will preserve that.
                  let extra = Map.fromList [("cps", (VF (coerce $! peCps pe))),
                                          ("delta", VF (T.addMicrosToOsc (peDelta pe) 0)),
                                          ("cycle", VF (fromRational (peCycle pe))) 
                                        ]
                      addExtra = Map.union playmap' extra
                      ts = (peOnWholeOrPartOsc pe) + nudge -- + latency
                  vs <- toData osc ((peEvent pe) {value = addExtra})
                  mungedPath <- substitutePath (path osc) playmap'
                  return (ts,
                          False, -- bus message ?
                          O.Message mungedPath vs
                          )
                | otherwise = Nothing
        toBus n | null busses = n
                | otherwise = busses !!! n
        busmsgs = map
                    (\(('^':k), (VI b)) -> do v <- Map.lookup k playmap
                                              return $ (tsPart,
                                                        True, -- bus message ?
                                                        O.Message "/c_set" [O.int32 b, toDatum v]
                                                      )
                    )
                    (Map.toList busmap)
          where
            tsPart = (peOnPartOsc pe) + nudge -- + latency
        nudge = fromJust $ getF $ fromMaybe (VF 0) $ Map.lookup "nudge" $ playmap
toOSC _ pe (OSCContext oscpath)
  = map cToM $ contextPosition $ context $ peEvent pe
  where cToM :: ((Int,Int),(Int,Int)) -> (Double, Bool, O.Message)
        cToM ((x, y), (x',y')) = (ts,
                                  False, -- bus message ?
                                  O.Message oscpath $ (O.string ident):(O.float (peDelta pe)):(O.float cyc):(map O.int32 [x,y,x',y'])
                                 )
        cyc :: Double
        cyc = fromRational $ peCycle pe
        nudge = fromMaybe 0 $ Map.lookup "nudge" (value $ peEvent pe) >>= getF
        ident = fromMaybe "unknown" $ Map.lookup "_id_" (value $ peEvent pe) >>= getS
        ts = (peOnWholeOrPartOsc pe) + nudge -- + latency

-- send has three modes:
-- Send events early using timestamp in the OSC bundle - used by Superdirt
-- Send events early by adding timestamp to the OSC message - used by Dirt
-- Send events live by delaying the thread
send :: Maybe O.UDP -> Cx -> Double -> Double -> (Double, Bool, O.Message) -> IO ()
send listen cx latency extraLatency (time, isBusMsg, m)
  | oSchedule target == Pre BundleStamp = sendBndl isBusMsg listen cx $ O.Bundle timeWithLatency [m]
  | oSchedule target == Pre MessageStamp = sendO isBusMsg listen cx $ addtime m
  | otherwise = do _ <- forkOS $ do now <- O.time
                                    threadDelay $ floor $ (timeWithLatency - now) * 1000000
                                    sendO isBusMsg listen cx m
                   return ()
    where addtime (O.Message mpath params) = O.Message mpath ((O.int32 sec):((O.int32 usec):params))
          ut = O.ntpr_to_ut timeWithLatency
          sec :: Int
          sec = floor ut
          usec :: Int
          usec = floor $ 1000000 * (ut - (fromIntegral sec))
          target = cxTarget cx
          timeWithLatency = time - latency + extraLatency

busResponder :: Int -> Cx -> IO ()
busResponder waits cx
  = do ms <- recvMessagesTimeout 2 (cxUDP cx)
       if (null ms)
         then do checkHandshake -- there was a timeout, check handshake
                 busResponder (waits+1) cx
         else do mapM_ act ms
                 busResponder 0 cx
     where
        checkHandshake = do busses <- readMVar (cxBusses cx)
                            when (null busses) $ do when  (waits == 0) $ verboseCx cx $ "Waiting for SuperDirt (v.1.7.2 or higher).."
                                                    sendHandshake cx

        act (O.Message "/dirt/hello" _) = sendHandshake cx
        act (O.Message "/dirt/handshake/reply" xs) = do prev <- swapMVar (cxBusses cx) $ bufferIndices xs
                                                        -- Only report the first time..
                                                        when (null prev) $ verboseCx cx $ "Connected to SuperDirt."
                                                        return ()
          where 
            bufferIndices [] = []
            bufferIndices (x:xs') | x == (O.ASCII_String $ O.ascii "&controlBusIndices") = catMaybes $ takeWhile isJust $ map O.datum_integral xs'
                                  | otherwise = bufferIndices xs'
        act m = hPutStrLn stderr $ "Unhandled OSC: " ++ show m

verboseCx :: Cx -> String -> IO ()
verboseCx c s = when (cxVerbose c) $ putStrLn s

recvMessagesTimeout :: (O.Transport t) => Double -> t -> IO [O.Message]
recvMessagesTimeout n sock = fmap (maybe [] O.packetMessages) $ O.recvPacketTimeout n sock