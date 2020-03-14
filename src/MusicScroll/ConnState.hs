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
import MusicScroll.UIEvent (UIEvent)
import MusicScroll.DBusNames

data ConnState = ConnState
  { cClient        :: Client
  , cBusActive     :: BusName
  , cOutTrackChan  :: TBQueue TrackInfo
  , cOutEventChan  :: TBQueue UIEvent
  , cLastSentTrack :: Maybe TrackInfo
  }

newConnState :: TBQueue TrackInfo -> TBQueue UIEvent -> Client -> ConnState
newConnState trackChan eventChan c =
  ConnState c vlcBus trackChan eventChan Nothing

setBus :: BusName -> ConnState -> ConnState
setBus newBus conn = conn { cBusActive = newBus }

setSong :: TrackInfo -> ConnState -> ConnState
setSong track s = s { cLastSentTrack = pure track }
