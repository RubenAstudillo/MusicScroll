{-# language LambdaCase #-}
module MusicScroll.MPRIS (dbusThreadP) where

import Control.Exception (bracket)
import Control.Monad (forever)
import Control.Monad.Trans.State (StateT, evalStateT)
import DBus.Client
import Pipes as P
import Pipes.Concurrent

import MusicScroll.TrackInfo
import MusicScroll.DBusSignals
import MusicScroll.ConnState
import MusicScroll.UIEvent

dbusThreadP :: Output TrackIdentifier -> Output ErrorCause -> IO a
dbusThreadP trackout errorout = bracket connectSession disconnect
  (evalStateT loop . newConnState)
  where
    loop :: StateT ConnState IO a
    loop = forever $ tryGetInfoP >>= \case
        Left (NoMusicClient _) -> changeMusicClientP
        Left NoSong ->
          do runEffect $ yield ENoSong >-> toOutput errorout
             waitForChangeP mediaPropChangeRule
        Right trackIdent ->
          do runEffect $ yield trackIdent >-> toOutput trackout
             waitForChangeP mediaPropChangeRule
