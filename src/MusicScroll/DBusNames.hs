{-# language OverloadedStrings #-}
module MusicScroll.DBusNames where

import DBus (BusName, ObjectPath, InterfaceName)

smplayerBus, vlcBus, dbusBus :: BusName
smplayerBus = "org.mpris.MediaPlayer2.smplayer"
vlcBus      = "org.mpris.MediaPlayer2.vlc"
dbusBus     = "org.freedesktop.DBus"

-- TODO: Add more!
allBuses :: [BusName]
allBuses = [smplayerBus, vlcBus]

mediaObject :: ObjectPath
mediaObject = "/org/mpris/MediaPlayer2"

mediaInterface :: InterfaceName
mediaInterface = "org.mpris.MediaPlayer2.Player"
