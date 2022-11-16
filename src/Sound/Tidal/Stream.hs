{-# LANGUAGE ConstraintKinds, GeneralizedNewtypeDeriving, FlexibleContexts, ScopedTypeVariables, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# language DeriveGeneric, StandaloneDeriving #-}

module Sound.Tidal.Stream (module Sound.Tidal.Stream) where

{-
    Stream.hs - Tidal's thingie for turning patterns into OSC streams
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

import           Control.Concurrent.MVar
import           Control.Concurrent
import           Control.Monad (forM_, when, forever)
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Data.Sequence (Seq, (|>), fromList, lookup, update)
import qualified Control.Exception as E
import           Foreign.C.Types
import           System.IO (hPutStrLn, stderr)

import qualified Sound.OSC.FD as O
import qualified Network.Socket          as N

import           Sound.Tidal.Config
import           Sound.Tidal.Core (stack, (#))
import           Sound.Tidal.ID
import qualified Sound.Tidal.Link as Link
import qualified Sound.Tidal.OSC.Target as New
import           Sound.Tidal.Params (pS)
import           Sound.Tidal.Pattern
import           Sound.Tidal.Target.Legacy
import qualified Sound.Tidal.Tempo as T
import           Data.List (sortOn)
import           System.Random (getStdRandom, randomR)
import           Sound.Tidal.Show ()

import           Sound.Tidal.Version

import Sound.Tidal.StreamTypes as Sound.Tidal.Stream

data Stream = Stream {sConfig :: Config,
                      sStateMV :: MVar ValueMap,
                      -- sOutput :: MVar ControlPattern,
                      sLink :: Link.AbletonLink,
                      sListen :: Maybe O.UDP,
                      sPMapMV :: MVar PlayMap,
                      sActionsMV :: MVar [T.TempoAction],
                      sGlobalFMV :: MVar (ControlPattern -> ControlPattern),
                      sTargets :: MVar (Seq New.GenericTarget)
                     }

defaultCps :: O.Time
defaultCps = 0.5625

-- Start an instance of Tidal
-- Spawns a thread within Tempo that acts as the clock
-- Spawns a thread that listens to and acts on OSC control messages
startStream :: Config -> [(Target, [OSC])] -> IO Stream
startStream config oscmap 
  = do sMapMV <- newMVar Map.empty
       pMapMV <- newMVar Map.empty
       globalFMV <- newMVar id
       actionsMV <- newEmptyMVar

       tidal_status_string >>= verbose config
       verbose config $ "Listening for external controls on " ++ cCtrlAddr config ++ ":" ++ show (cCtrlPort config)
       listen <- openListener config

       cxs <- mapM (legacyCx config) oscmap
       targets <- newMVar $ Data.Sequence.fromList $ map New.GenericTarget cxs

       let bpm = (coerce defaultCps) * 60 * (cBeatsPerCycle config)
       abletonLink <- Link.create bpm
       let stream = Stream {sConfig = config,
                            sStateMV  = sMapMV,
                            sLink = abletonLink,
                            sListen = listen,
                            sPMapMV = pMapMV,
                            sActionsMV = actionsMV,
                            sGlobalFMV = globalFMV,
                            sTargets = targets
                           }
       let ac = T.ActionHandler {
         T.onTick = onTick stream,
         T.onSingleTick = onSingleTick stream,
         T.updatePattern = updatePattern stream
         }
       -- Spawn a thread that acts as the clock
       _ <- T.clocked config sMapMV pMapMV actionsMV ac abletonLink
       -- Spawn a thread to handle OSC control messages
       _ <- forkIO (ctrlResponder stream)
       return stream

addTarget :: New.Target a => Stream -> a -> IO Int
addTarget Stream{sTargets=ts} t = New.startTarget t >> length <$> withMVar ts append
  where
    append :: Seq New.GenericTarget -> IO (Seq New.GenericTarget)
    append tseq = return $ tseq |> (New.GenericTarget t)

removeTarget :: Stream -> Int -> IO ()
removeTarget Stream {sTargets=ts} i = modifyMVar_ ts endAndRemove
  where
    endAndRemove :: Seq New.GenericTarget -> IO (Seq New.GenericTarget)
    endAndRemove tseq = endTarget (Data.Sequence.lookup i tseq) >> (emptyTarget tseq)
    endTarget :: Maybe New.GenericTarget -> IO ()
    endTarget (Just (New.GenericTarget t)) = New.endTarget t
    endTarget _ = return ()
    emptyTarget :: Seq New.GenericTarget -> IO (Seq New.GenericTarget)
    emptyTarget tseq = return (update i New.EmptyTarget tseq)

-- Start an instance of Tidal with superdirt OSC
startTidal :: Target -> Config -> IO Stream
startTidal target config = startStream config [(target, [superdirtShape])]

startMulti :: [Target] -> Config -> IO ()
startMulti _ _ = hPutStrLn stderr $ "startMulti has been removed, please check the latest documentation on tidalcycles.org"

playStack :: PlayMap -> ControlPattern
playStack pMap = stack $ map pattern active
  where active = filter (\pState -> if hasSolo pMap
                                    then solo pState
                                    else not (mute pState)
                        ) $ Map.elems pMap

-- Used for Tempo callback
updatePattern :: Stream -> ID -> ControlPattern -> IO ()
updatePattern stream k pat = do
  let x = queryArc pat (Arc 0 0)
  pMap <- seq x $ takeMVar (sPMapMV stream)
  let playState = updatePS $ Map.lookup (fromID k) pMap
  putMVar (sPMapMV stream) $ Map.insert (fromID k) playState pMap
  where updatePS (Just playState) = do playState {pattern = pat', history = pat:(history playState)}
        updatePS Nothing = PlayState pat' False False [pat']
        pat' = pat # pS "_id_" (pure $ fromID k)

processCps :: T.LinkOperations -> [Event ValueMap] -> IO [ProcessedEvent]
processCps ops = mapM processEvent
  where
    processEvent ::  Event ValueMap  -> IO ProcessedEvent
    processEvent e = do
      let wope = wholeOrPart e
          partStartCycle = start $ part e
          partStartBeat = (T.cyclesToBeat ops) (realToFrac partStartCycle)
          onCycle = start wope
          onBeat = (T.cyclesToBeat ops) (realToFrac onCycle)
          offCycle = stop wope
          offBeat = (T.cyclesToBeat ops) (realToFrac offCycle)
      on <- (T.timeAtBeat ops) onBeat
      onPart <- (T.timeAtBeat ops) partStartBeat
      when (eventHasOnset e) (do
        let cps' = Map.lookup "cps" (value e) >>= getF
        maybe (return ()) (\newCps -> (T.setTempo ops) ((T.cyclesToBeat ops) (newCps * 60)) on) $ coerce cps' 
        )
      off <- (T.timeAtBeat ops) offBeat
      bpm <- (T.getTempo ops)
      let cps = ((T.beatToCycles ops) bpm) / 60
      let delta = off - on
      return $! ProcessedEvent {
          peHasOnset = eventHasOnset e,
          peEvent = e,
          peCps = cps,
          peDelta = delta,
          peCycle = onCycle,
          peOnWholeOrPart = on,
          peOnWholeOrPartOsc = (T.linkToOscTime ops) on,
          peOnPart = onPart,
          peOnPartOsc = (T.linkToOscTime ops) onPart
        }


-- streamFirst but with random cycle instead of always first cicle
streamOnce :: Stream -> ControlPattern -> IO ()
streamOnce st p = do i <- getStdRandom $ randomR (0, 8192)
                     streamFirst st $ rotL (toRational (i :: Int)) p

-- here let's do modifyMVar_ on actions
streamFirst :: Stream -> ControlPattern -> IO ()
streamFirst stream pat = modifyMVar_ (sActionsMV stream) (\actions -> return $ (T.SingleTick pat) : actions)

-- Used for Tempo callback
onTick :: Stream -> TickState -> T.LinkOperations -> ValueMap -> IO ValueMap
onTick stream st ops s
  = doTick stream st ops s

-- Used for Tempo callback
-- Tempo changes will be applied.
-- However, since the full arc is processed at once and since Link does not support
-- scheduling, tempo change may affect scheduling of events that happen earlier
-- in the normal stream (the one handled by onTick).
onSingleTick :: Stream -> T.LinkOperations -> ValueMap -> ControlPattern -> IO ValueMap
onSingleTick stream ops s pat = do
  pMapMV <- newMVar $ Map.singleton "fake"
          (PlayState {pattern = pat,
                      mute = False,
                      solo = False,
                      history = []
                      }
          )

  -- The nowArc is a full cycle
  let state = TickState {tickArc = (Arc 0 1), tickNudge = 0}
  doTick (stream {sPMapMV = pMapMV}) state ops s


-- | Query the current pattern (contained in argument @stream :: Stream@)
-- for the events in the current arc (contained in argument @st :: T.State@),
-- translate them to OSC messages, and send these.
--
-- If an exception occurs during sending,
-- this functions prints a warning and continues, because
-- the likely reason is that the backend (supercollider) isn't running.
-- 
-- If any exception occurs before or outside sending
-- (e.g., while querying the pattern, while computing a message),
-- this function prints a warning and resets the current pattern
-- to the previous one (or to silence if there isn't one) and continues,
-- because the likely reason is that something is wrong with the current pattern.
doTick :: Stream -> TickState -> T.LinkOperations -> ValueMap -> IO ValueMap
doTick stream st ops sMap =
  E.handle (\ (e :: E.SomeException) -> do
    hPutStrLn stderr $ "Failed to Stream.doTick: " ++ show e
    hPutStrLn stderr $ "Return to previous pattern."
    setPreviousPatternOrSilence stream
    return sMap) (do
      pMap <- readMVar (sPMapMV stream)
      sGlobalF <- readMVar (sGlobalFMV stream)
      bpm <- (T.getTempo ops)
      targets <- readMVar (sTargets stream)
      let
        patstack = sGlobalF $ playStack pMap
        cps = ((T.beatToCycles ops) bpm) / 60
        sMap' = Map.insert "_cps" (VF $ coerce cps) sMap
        nudge = tickNudge st
        -- First the state is used to query the pattern
        es = sortOn (start . part) $ query patstack (State {arc = tickArc st,
                                                        controls = sMap'
                                                      }
                                                )
         -- Then it's passed through the events
        (sMap'', es') = resolveState sMap' es
      tes <- processCps ops es'
      -- For each OSC target
      let tickTarget (New.GenericTarget t) = forM_ tes (New.tickTarget t nudge)
          tickTarget (New.EmptyTarget) = return ()
      forM_ targets tickTarget
      sMap'' `seq` return sMap'')

setPreviousPatternOrSilence :: Stream -> IO ()
setPreviousPatternOrSilence stream =
  modifyMVar_ (sPMapMV stream) $ return
    . Map.map ( \ pMap -> case history pMap of
      _:p:ps -> pMap { pattern = p, history = p:ps }
      _ -> pMap { pattern = silence, history = [silence] }
              )

-- Interaction

streamNudgeAll :: Stream -> Double -> IO ()
streamNudgeAll s nudge = T.setNudge (sActionsMV s) nudge

streamResetCycles :: Stream -> IO ()
streamResetCycles s =T.resetCycles (sActionsMV s)

hasSolo :: Map.Map k PlayState -> Bool
hasSolo = (>= 1) . length . filter solo . Map.elems

streamList :: Stream -> IO ()
streamList s = do pMap <- readMVar (sPMapMV s)
                  let hs = hasSolo pMap
                  putStrLn $ concatMap (showKV hs) $ Map.toList pMap
  where showKV :: Bool -> (PatId, PlayState) -> String
        showKV True  (k, (PlayState {solo = True})) = k ++ " - solo\n"
        showKV True  (k, _) = "(" ++ k ++ ")\n"
        showKV False (k, (PlayState {solo = False})) = k ++ "\n"
        showKV False (k, _) = "(" ++ k ++ ") - muted\n"

-- Evaluation of pat is forced so exceptions are picked up here, before replacing the existing pattern.

streamReplace :: Stream -> ID -> ControlPattern -> IO ()
streamReplace s k !pat
  = modifyMVar_ (sActionsMV s) (\actions -> return $ (T.StreamReplace k pat) : actions)

streamMute :: Stream -> ID -> IO ()
streamMute s k = withPatIds s [k] (\x -> x {mute = True})

streamMutes :: Stream -> [ID] -> IO ()
streamMutes s ks = withPatIds s ks (\x -> x {mute = True})

streamUnmute :: Stream -> ID -> IO ()
streamUnmute s k = withPatIds s [k] (\x -> x {mute = False})

streamSolo :: Stream -> ID -> IO ()
streamSolo s k = withPatIds s [k] (\x -> x {solo = True})

streamUnsolo :: Stream -> ID -> IO ()
streamUnsolo s k = withPatIds s [k] (\x -> x {solo = False})

withPatIds :: Stream -> [ID] -> (PlayState -> PlayState) -> IO ()
withPatIds s ks f
  = do playMap <- takeMVar $ sPMapMV s
       let pMap' = foldr (Map.update (\x -> Just $ f x)) playMap (map fromID ks)
       putMVar (sPMapMV s) pMap'
       return ()

-- TODO - is there a race condition here?
streamMuteAll :: Stream -> IO ()
streamMuteAll s = modifyMVar_ (sPMapMV s) $ return . fmap (\x -> x {mute = True})

streamHush :: Stream -> IO ()
streamHush s = modifyMVar_ (sPMapMV s) $ return . fmap (\x -> x {pattern = silence, history = silence:history x})

streamUnmuteAll :: Stream -> IO ()
streamUnmuteAll s = modifyMVar_ (sPMapMV s) $ return . fmap (\x -> x {mute = False})

streamUnsoloAll :: Stream -> IO ()
streamUnsoloAll s = modifyMVar_ (sPMapMV s) $ return . fmap (\x -> x {solo = False})

streamSilence :: Stream -> ID -> IO ()
streamSilence s k = withPatIds s [k] (\x -> x {pattern = silence, history = silence:history x})

streamAll :: Stream -> (ControlPattern -> ControlPattern) -> IO ()
streamAll s f = do _ <- swapMVar (sGlobalFMV s) f
                   return ()

streamGet :: Stream -> String -> IO (Maybe Value)
streamGet s k = Map.lookup k <$> readMVar (sStateMV s)

streamSet :: Valuable a => Stream -> String -> Pattern a -> IO ()
streamSet s k pat = do sMap <- takeMVar $ sStateMV s
                       let pat' = toValue <$> pat
                           sMap' = Map.insert k (VPattern pat') sMap
                       putMVar (sStateMV s) $ sMap'

streamSetI :: Stream -> String -> Pattern Int -> IO ()
streamSetI = streamSet

streamSetF :: Stream -> String -> Pattern Double -> IO ()
streamSetF = streamSet

streamSetS :: Stream -> String -> Pattern String -> IO ()
streamSetS = streamSet

streamSetB :: Stream -> String -> Pattern Bool -> IO ()
streamSetB = streamSet

streamSetR :: Stream -> String -> Pattern Rational -> IO ()
streamSetR = streamSet

openListener :: Config -> IO (Maybe O.UDP)
openListener c
  | cCtrlListen c = catchAny run (\_ -> do verbose c "That port isn't available, perhaps another Tidal instance is already listening on that port?"
                                           return Nothing
                                 )
  | otherwise  = return Nothing
  where
        run = do sock <- O.udpServer (cCtrlAddr c) (cCtrlPort c)
                 when (cCtrlBroadcast c) $ N.setSocketOption (O.udpSocket sock) N.Broadcast 1
                 return $ Just sock
        catchAny :: IO a -> (E.SomeException -> IO a) -> IO a
        catchAny = E.catch

-- Listen to and act on OSC control messages
ctrlResponder :: Stream -> IO ()
ctrlResponder (stream@(Stream {sListen = Just sock}))
  = do forever $ recvMessages sock >>= (mapM_ act)
     where
        -- External controller commands
        act (O.Message "/ctrl" (O.Int32 k:v:[]))
          = act (O.Message "/ctrl" [O.string $ show k,v])
        act (O.Message "/ctrl" (O.ASCII_String k:v@(O.Float _):[]))
          = add (O.ascii_to_string k) (VF (fromJust $ O.datum_floating v))
        act (O.Message "/ctrl" (O.ASCII_String k:O.ASCII_String v:[]))
          = add (O.ascii_to_string k) (VS (O.ascii_to_string v))
        act (O.Message "/ctrl" (O.ASCII_String k:O.Int32 v:[]))
          = add (O.ascii_to_string k) (VI (fromIntegral v))
        -- Stream playback commands
        act (O.Message "/mute" (k:[]))
          = withID k $ streamMute stream
        act (O.Message "/unmute" (k:[]))
          = withID k $ streamUnmute stream
        act (O.Message "/solo" (k:[]))
          = withID k $ streamSolo stream
        act (O.Message "/unsolo" (k:[]))
          = withID k $ streamUnsolo stream
        act (O.Message "/muteAll" [])
          = streamMuteAll stream
        act (O.Message "/unmuteAll" [])
          = streamUnmuteAll stream
        act (O.Message "/unsoloAll" [])
          = streamUnsoloAll stream
        act (O.Message "/hush" [])
          = streamHush stream
        act (O.Message "/silence" (k:[]))
          = withID k $ streamSilence stream
        act m = hPutStrLn stderr $ "Unhandled OSC: " ++ show m
        add :: String -> Value -> IO ()
        add k v = do sMap <- takeMVar (sStateMV stream)
                     putMVar (sStateMV stream) $ Map.insert k v sMap
                     return ()
        withID :: O.Datum -> (ID -> IO ()) -> IO ()
        withID (O.ASCII_String k) func = func $ (ID . O.ascii_to_string) k
        withID (O.Int32 k) func = func $ (ID . show) k
        withID _ _ = return ()
ctrlResponder _ = return ()

verbose :: Config -> String -> IO ()
verbose c s = when (cVerbose c) $ putStrLn s

recvMessages :: O.UDP -> IO [O.Message]
recvMessages sock = fmap O.packetMessages (O.udp_recv_packet sock)

streamGetcps :: Stream -> IO Double
streamGetcps s = do
  let config = sConfig s
  ss <- Link.createAndCaptureAppSessionState (sLink s)
  bpm <- Link.getTempo ss
  return $! coerce $ bpm / (cBeatsPerCycle config) / 60

streamGetnow :: Stream -> IO Double
streamGetnow s = do
  let config = sConfig s
  ss <- Link.createAndCaptureAppSessionState (sLink s)
  now <- Link.clock (sLink s)
  beat <- Link.beatAtTime ss now (cQuantum config)
  return $! coerce $ beat / (cBeatsPerCycle config)
