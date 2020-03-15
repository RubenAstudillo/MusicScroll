module MusicScroll.RealMain (realMain) where

import Control.Concurrent.Async (withAsync, waitAnyCancel)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (newTBQueue)
import Data.Functor (void)

import MusicScroll.LyricsPipeline
import MusicScroll.MPRIS
import MusicScroll.UI

realMain :: IO ()
realMain =
  do dbusSongChan <- atomically (newTBQueue sizeOfQueue)
     eventChan    <- atomically (newTBQueue sizeOfQueue)
     suplChan     <- atomically (newTBQueue sizeOfQueue)
     withAsync (setupUIThread eventChan suplChan) $ \setupUIA ->
       withAsync (lyricsThread dbusSongChan suplChan eventChan) $ \lyricsA ->
         withAsync (dbusThread dbusSongChan eventChan) $ \dbusA ->
           void $ waitAnyCancel [setupUIA, lyricsA, dbusA]
