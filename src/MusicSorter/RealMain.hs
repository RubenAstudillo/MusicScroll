{-# language OverloadedStrings #-}
module MusicSorter.RealMain (realMain) where

import Control.Concurrent.Async (withAsync, wait)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (newTBQueue)
import Control.Concurrent.STM.TMVar (newEmptyTMVar)
import Numeric.Natural (Natural)

import MusicSorter.AZLyrics
import MusicSorter.MPRIS
import MusicSorter.UI

realMain :: IO ()
realMain =
  do dbusSongChan <- atomically (newTBQueue sizeOfQueue)
     lyricsChan   <- atomically (newTBQueue sizeOfQueue)
     userInterruptMVar <- atomically newEmptyTMVar
     withAsync (setupUIThread lyricsChan) $ \setupUIA ->
       withAsync (lyricsThread dbusSongChan lyricsChan) $ \lyricsA ->
         withAsync (dbusThread userInterruptMVar dbusSongChan) $ \dbusA ->
           wait setupUIA *> wait lyricsA *> wait dbusA *> pure ()

sizeOfQueue :: Natural
sizeOfQueue = 5
