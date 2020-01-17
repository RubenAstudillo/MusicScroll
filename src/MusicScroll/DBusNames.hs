{-# language OverloadedStrings #-}
module MusicScroll.DBusNames where

import DBus (BusName, ObjectPath, InterfaceName)

smplayerBus :: BusName
smplayerBus = "org.mpris.MediaPlayer2.smplayer"

mediaObject :: ObjectPath
mediaObject = "/org/mpris/MediaPlayer2"

mediaInterface :: InterfaceName
mediaInterface = "org.mpris.MediaPlayer2.Player"
