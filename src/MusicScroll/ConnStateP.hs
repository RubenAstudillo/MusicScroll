module MusicScroll.ConnStateP where

import DBus (BusName)
import DBus.Client (Client)
import MusicScroll.DBusNames

data ConnStateP = ConnStateP
  { cpClient        :: Client
  , cpBusActive     :: BusName
  }

newConnStateP :: Client -> ConnStateP
newConnStateP c = ConnStateP c vlcBus
