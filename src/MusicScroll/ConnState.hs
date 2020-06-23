module MusicScroll.ConnState where

import DBus (BusName)
import DBus.Client (Client)
import Control.Concurrent.STM.TBQueue (TBQueue)
import MusicScroll.TrackInfo (TrackIdentifier)
import MusicScroll.UIEvent (UIEvent)
import MusicScroll.DBusNames

data ConnState = ConnState
  { cClient        :: Client
  , cBusActive     :: BusName
  , cOutTrackChan  :: TBQueue TrackIdentifier
  , cOutEventChan  :: TBQueue UIEvent
  }

newConnState :: TBQueue TrackIdentifier -> TBQueue UIEvent
             -> Client -> ConnState
newConnState trackCh eventCh c = ConnState c vlcBus trackCh eventCh

setBus :: BusName -> ConnState -> ConnState
setBus newBus conn = conn { cBusActive = newBus }
