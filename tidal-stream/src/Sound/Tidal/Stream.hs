{-# LANGUAGE ExistentialQuantification #-}

module Sound.Tidal.Stream where

import Control.Concurrent.MVar
import Data.Map.Strict (Map)

import Sound.Tidal.ID

data Stream = Stream {
  sContexts :: MVar (Map ID ContextWrapper)
}

class Extension e where
  start :: Context c => e -> IO c

class (Eq a, Show a) => Context a where
  listen :: Maybe (a -> Stream -> IO ())
  panic :: Maybe (a -> Stream -> IO ())
  send :: Maybe (a -> [()] -> IO ())
  stop :: a -> IO ()

data ContextWrapper = forall a. Context a => Context a deriving Eq

instance Show ContextWrapper where
  show (Context a) = show a

streamAttach :: (Extension e, Context c) => Stream -> e -> ID -> IO c
streamAttach _ ext _ = start ext