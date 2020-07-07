module MusicScroll.ConnState where

import DBus (BusName)
import DBus.Client (Client)
import MusicScroll.DBusNames

data ConnState = ConnState
  { cpClient        :: Client
  , cpBusActive     :: BusName
  }

newConnState :: Client -> ConnState
newConnState c = ConnState c vlcBus
