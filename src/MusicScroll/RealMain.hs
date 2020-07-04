{-# language ScopedTypeVariables #-}
module MusicScroll.RealMain (realMain, realMain2) where

import Control.Concurrent.Async (withAsync, withAsyncBound, waitAnyCancel)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (newTBQueue, TBQueue)
import Control.Concurrent.STM.TMVar
import Control.Concurrent.MVar
import Data.Functor (void)
import Control.Exception (bracket, catch, SomeException(..))
import Database.SQLite.Simple
import Pipes.Concurrent

import MusicScroll.LyricsPipeline
import MusicScroll.MPRIS
import MusicScroll.UI
import MusicScroll.EventLoop
import MusicScroll.DatabaseUtils (getDBPath)

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
realMain2 = do
  appCtxTMvar  <- atomically newEmptyTMVar
  uiCallbackTB <- atomically (newTBQueue sizeOfQueue)
  withAsyncBound (uiThread2 appCtxTMvar) $ \uiA -> do
    (outTrack, inTrack) <- spawn (newest 1)
    (outErr, inErr)     <- spawn (newest 1)
    withAsync (dbusThreadP outTrack outErr) $ \dbusA -> do
      dbPath <- getDBPath
      bracket (open dbPath) close $ \conn -> do
        mconn <- newMVar conn
        ctx   <- atomically (takeTMVar appCtxTMvar)
        let state = AppState ctx mconn inTrack inErr
        let evState = EventLoopState state uiCallbackTB Nothing
        withAsync (staticPipeline state) $ \staticA ->
          withAsync (eventLoop evState) $ \evLoopA ->
            void $ waitAnyCancel [ staticA, evLoopA, uiA, dbusA ]
