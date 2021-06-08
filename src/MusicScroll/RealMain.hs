{-# LANGUAGE ScopedTypeVariables #-}

module MusicScroll.RealMain (realMain) where

import Control.Concurrent.Async (waitAnyCancel, withAsync, withAsyncBound)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TBQueue (newTBQueue)
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import Control.Exception (bracket)
import Data.Functor (void)
import Database.SQLite.Simple
import MusicScroll.DatabaseUtils (getDBPath, sqlDBCreate)
import MusicScroll.EventLoop
import MusicScroll.MPRIS
import MusicScroll.Pipeline
import MusicScroll.UI
import Pipes.Concurrent

realMain :: IO ()
realMain = do
  appCtxTMvar <- atomically newEmptyTMVar
  suplTVar <- atomically (newTVar Nothing)
  uiCallbackTB <- atomically (newTBQueue 5)
  withAsyncBound (uiThread appCtxTMvar uiCallbackTB suplTVar) $ \uiA -> do
    (trackin, errorin, singleProd, trackout, errorout) <- musicSpawn
    withAsync (dbusThread trackout errorout) $ \dbusA -> do
      dbPath <- getDBPath
      bracket (open dbPath) close $ \conn -> do
        execute_ conn sqlDBCreate
        mconn <- newMVar conn
        ctx <- atomically (takeTMVar appCtxTMvar)
        let state = AppState ctx mconn suplTVar (trackin, errorin) singleProd
        let evState = EventLoopState state uiCallbackTB Nothing
        withAsync (staticPipeline state) $ \staticA ->
          withAsync (eventLoop evState) $ \evLoopA ->
            void $ waitAnyCancel [staticA, evLoopA, uiA, dbusA]
