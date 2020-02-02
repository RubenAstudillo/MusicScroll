module MusicScroll.ConnState
  ( ConnState(..)
  , newConnState
  , setSong
  , setBus
  ) where

import DBus (BusName)
import DBus.Client (Client)
import Control.Concurrent.STM.TBQueue (TBQueue)
import MusicScroll.TrackInfo

import MusicScroll.DBusNames

data ConnState = ConnState
  { cClient        :: Client
  , cBusActive     :: BusName
  , cOutChan       :: TBQueue TrackInfo
  , cLastSentTrack :: Maybe TrackInfo
  }

newConnState :: TBQueue TrackInfo -> Client -> ConnState
newConnState outChan c = ConnState c vlcBus outChan Nothing

setBus :: BusName -> ConnState -> ConnState
setBus newBus conn = conn { cBusActive = newBus }

setSong :: TrackInfo -> ConnState -> ConnState
setSong track s = s { cLastSentTrack = pure track }
