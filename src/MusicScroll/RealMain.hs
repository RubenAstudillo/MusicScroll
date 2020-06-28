module MusicScroll.RealMain (realMain, realMain2) where

import Control.Concurrent.Async (withAsync, withAsyncBound, waitAnyCancel)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (newTBQueue, TBQueue)
import Control.Concurrent.STM.TMVar
import Data.Functor (void)
import Control.Exception (bracket)
import Database.SQLite.Simple

import MusicScroll.LyricsPipeline
import MusicScroll.MPRIS
import MusicScroll.UI
import MusicScroll.EventLoop

realMain :: IO ()
realMain =
  do dbusSongChan <- atomically (newTBQueue sizeOfQueue)
     eventChan    <- atomically (newTBQueue sizeOfQueue)
     suplChan     <- atomically (newTBQueue sizeOfQueue)
     withAsync (setupUIThread eventChan suplChan) $ \setupUIA ->
       withAsync (lyricsThread (dbusSongChan, suplChan) eventChan) $ \lyricsA ->
         withAsync (dbusThread dbusSongChan eventChan) $ \dbusA ->
           void $ waitAnyCancel [setupUIA, lyricsA, dbusA]

realMain2 :: IO ()
realMain2 =
  do tmvar <- atomically newEmptyTMVar
     tbcallback <- atomically (newTBQueue sizeOfQueue) -- unused
     withAsyncBound (uiThread2 tmvar) $ \uiA -> do
       ctx <- atomically (takeTMVar tmvar)
       eventLoop ctx tbcallback
