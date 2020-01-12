{-# language OverloadedStrings #-}
module MusicSorter.RealMain (realMain) where

import           Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as A
import           Control.Concurrent.STM (STM, atomically)
import qualified Control.Concurrent.STM.TBQueue as TB
import           Control.Concurrent.STM.TChan (TChan)
import qualified Control.Concurrent.STM.TMVar as TM
import           MusicSorter.AZLyrics
import           MusicSorter.MPRIS
import           MusicSorter.UI

realMain :: IO ()
realMain =
  do dbusSongChan <- atomically (TB.newTBQueue 5)
     lyricsChan   <- atomically (TB.newTBQueue 5)
     userInterruptMVar   <- atomically TM.newEmptyTMVar
     A.withAsync (setupUIThread lyricsChan) $ \setupUIAsync ->
       A.withAsync (lyricsThread dbusSongChan lyricsChan) $ \lyricsA ->
         A.withAsync (dbusThread userInterruptMVar dbusSongChan) $ \dbusA ->
           A.wait setupUIAsync *> A.wait lyricsA *> A.wait dbusA *> pure ()
