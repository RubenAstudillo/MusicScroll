{-# language OverloadedStrings #-}
module MusicScroll.DBusNames where

import DBus (BusName, ObjectPath, InterfaceName)

smplayerBus, vlcBus :: BusName
smplayerBus = "org.mpris.MediaPlayer2.smplayer"
vlcBus      = "org.mpris.MediaPlayer2.vlc"

mediaObject :: ObjectPath
mediaObject = "/org/mpris/MediaPlayer2"

mediaInterface :: InterfaceName
mediaInterface = "org.mpris.MediaPlayer2.Player"
