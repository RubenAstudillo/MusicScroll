{-# language LambdaCase #-}
module MusicScroll.MPRIS (dbusThread) where

import Control.Exception (bracket)
import Control.Monad (forever)
import Control.Monad.Trans.State (StateT, evalStateT)
import DBus.Client
import Pipes as P
import Pipes.Concurrent

import MusicScroll.TrackInfo
import MusicScroll.DBusSignals
import MusicScroll.ConnState
import MusicScroll.LyricsPipeline

dbusThread :: Output TrackIdentifier -> Output ErrorCause -> IO a
dbusThread trackout errorout = bracket connectSession disconnect
  (evalStateT loop . newConnState)
  where
    loop :: StateT ConnState IO a
    loop = forever $ tryGetInfo >>= \case
        Left (NoMusicClient _) -> changeMusicClient
        Left NoSong ->
          do runEffect $ yield ENoSong >-> toOutput errorout
             waitForChange mediaPropChangeRule
        Right trackIdent ->
          do runEffect $ yield trackIdent >-> toOutput trackout
             waitForChange mediaPropChangeRule
