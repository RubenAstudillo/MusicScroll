module MusicScroll.ConnStateP where

import DBus (BusName)
import DBus.Client (Client)
import MusicScroll.DBusNames

data ConnStateP = ConnStateP
  { cpClient        :: Client
  , cpBusActive     :: BusName
  , iter :: Int
  }

newConnStateP :: Client -> ConnStateP
newConnStateP c = ConnStateP c vlcBus 0
