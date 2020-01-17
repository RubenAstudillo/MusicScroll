{-# language OverloadedStrings #-}
module MusicScroll.RealMain (realMain) where

import Control.Concurrent.Async (withAsync, waitAnyCancel)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (newTBQueue)
import Numeric.Natural (Natural)
import Data.Functor (void)

import MusicScroll.AZLyrics
import MusicScroll.MPRIS
import MusicScroll.UI

realMain :: IO ()
realMain =
  do dbusSongChan <- atomically (newTBQueue sizeOfQueue)
     lyricsChan   <- atomically (newTBQueue sizeOfQueue)
     withAsync (setupUIThread lyricsChan) $ \setupUIA ->
       withAsync (lyricsThread dbusSongChan lyricsChan) $ \lyricsA ->
         withAsync (dbusThread dbusSongChan) $ \dbusA ->
           void $ waitAnyCancel [setupUIA, lyricsA, dbusA]

sizeOfQueue :: Natural
sizeOfQueue = 5
